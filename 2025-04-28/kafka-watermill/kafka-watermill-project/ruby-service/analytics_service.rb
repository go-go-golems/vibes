require 'kafka'
require 'json'
require 'logger'
require_relative '../idl/ruby/order_events'

# Set up logging
logger = Logger.new(STDOUT)
logger.level = Logger::INFO

# Configure Kafka client
kafka = Kafka.new(
  seed_brokers: ['kafka:9092'],
  client_id: 'analytics-service'
)

# Create a consumer
consumer = kafka.consumer(group_id: 'analytics-service')

# Subscribe to all order-related topics
consumer.subscribe('order.created')
consumer.subscribe('payment.processed')
consumer.subscribe('inventory.checked')
consumer.subscribe('order.fulfilled')
consumer.subscribe('order.cancelled')

# Initialize analytics data
order_counts = {
  created: 0,
  payment_success: 0,
  payment_failed: 0,
  fulfilled: 0,
  cancelled: 0
}

# Create a separate thread for a simple HTTP server to report analytics
require 'sinatra'
require 'concurrent'

# Start the analytics processor
processor = Concurrent::Promise.execute do
  logger.info('Analytics service starting to consume events...')
  
  begin
    consumer.each_message do |message|
      topic = message.topic
      value = message.value

      begin
        case topic
        when 'order.created'
          event = KafkaWatermill::IDL::OrderCreated.from_json(value)
          order_counts[:created] += 1
          logger.info("Order created event processed: #{event.order_id}")

        when 'payment.processed'
          event = KafkaWatermill::IDL::PaymentProcessed.from_json(value)
          if event.status == 'success'
            order_counts[:payment_success] += 1
            logger.info("Payment success event processed: #{event.order_id}")
          else
            order_counts[:payment_failed] += 1
            logger.info("Payment failed event processed: #{event.order_id}")
          end

        when 'order.fulfilled'
          event = KafkaWatermill::IDL::OrderFulfilled.from_json(value)
          order_counts[:fulfilled] += 1
          logger.info("Order fulfilled event processed: #{event.order_id}")

        when 'order.cancelled'
          event = KafkaWatermill::IDL::OrderCancelled.from_json(value)
          order_counts[:cancelled] += 1
          logger.info("Order cancelled event processed: #{event.order_id}")
        end

        # Log aggregated metrics periodically
        if (order_counts[:created] % 5) == 0
          logger.info("ANALYTICS REPORT: Created: #{order_counts[:created]}, " +
                      "Payment Success: #{order_counts[:payment_success]}, " +
                      "Payment Failed: #{order_counts[:payment_failed]}, " +
                      "Fulfilled: #{order_counts[:fulfilled]}, " +
                      "Cancelled: #{order_counts[:cancelled]}")
        end
      rescue => e
        logger.error("Error processing message from topic #{topic}: #{e.message}")
        logger.error(e.backtrace.join("\n"))
      end
    end
  rescue => e
    logger.error("Kafka consumer error: #{e.message}")
    logger.error(e.backtrace.join("\n"))
  end
end

# Define a simple REST API for the analytics service
class AnalyticsAPI < Sinatra::Base
  set :bind, '0.0.0.0'
  set :port, 3000

  get '/analytics' do
    content_type :json
    {
      order_counts: order_counts,
      timestamp: Time.now.iso8601
    }.to_json
  end

  get '/health' do
    content_type :json
    { status: 'UP' }.to_json
  end
end

# Start the Sinatra app
AnalyticsAPI.run!

# Wait for the processor to complete (it shouldn't unless there's an error)
processor.wait

# Ensure proper shutdown
at_exit do
  logger.info('Shutting down analytics service...')
  consumer.stop
end