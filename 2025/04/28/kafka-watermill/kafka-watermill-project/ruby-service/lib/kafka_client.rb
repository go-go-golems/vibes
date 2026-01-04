require 'kafka'
require 'logger'
require 'concurrent'
require_relative '../../idl/ruby/order_events'

module AnalyticsService
  class KafkaClient
    attr_reader :logger, :kafka, :consumer, :producer

    def initialize(config = {})
      config = default_config.merge(config)
      @logger = config[:logger]
      
      # Configure Kafka client
      @kafka = Kafka.new(
        seed_brokers: config[:brokers],
        client_id: config[:client_id],
        logger: @logger
      )

      # Create consumer and producer
      @consumer = create_consumer(config[:consumer_group])
      @producer = @kafka.producer(
        required_acks: config[:required_acks],
        compression_codec: config[:compression_codec],
        max_retries: config[:max_retries],
        retry_backoff: config[:retry_backoff]
      )

      # Subscribe to topics
      Array(config[:subscribe_topics]).each do |topic|
        @consumer.subscribe(topic)
      end

      # Set up delivery reports callback if provided
      if config[:delivery_callback]
        @producer.delivery_callback = config[:delivery_callback]
      end

      # Set up automatic producer shutdown on exit
      at_exit do
        begin
          @logger.info("Shutting down Kafka producer...")
          @producer.shutdown
        rescue => e
          @logger.error("Error shutting down producer: #{e.message}")
        end
      end
    end

    def start_consumer(processor)
      Concurrent::Promise.execute do
        @logger.info("Starting Kafka consumer with processor: #{processor.class.name}")
        begin
          @consumer.each_message do |message|
            begin
              processor.process_message(message)
            rescue => e
              @logger.error("Error processing message from topic #{message.topic}: #{e.message}")
              @logger.error(e.backtrace.join("\n"))
            end
          end
        rescue => e
          @logger.error("Kafka consumer error: #{e.message}")
          @logger.error(e.backtrace.join("\n"))
        end
      end
    end

    def produce_message(topic, key, value, partition_key = nil)
      @producer.produce(
        value,
        topic: topic,
        key: key,
        partition_key: partition_key || key
      )
      @producer.deliver_messages
    end

    def stop_consumer
      @consumer.stop
    end

    private

    def create_consumer(group_id)
      @kafka.consumer(
        group_id: group_id,
        offset_commit_interval: 5,
        offset_commit_threshold: 100,
        fetcher_max_queue_size: 10,
        heartbeat_interval: 10
      )
    end

    def default_config
      {
        brokers: ['kafka:9092'],
        client_id: 'analytics-service',
        consumer_group: 'analytics-service-group',
        subscribe_topics: [
          'order.created',
          'payment.processed',
          'inventory.checked',
          'order.fulfilled',
          'order.cancelled'
        ],
        required_acks: :all,
        compression_codec: :snappy,
        max_retries: 5,
        retry_backoff: 5,
        logger: Logger.new(STDOUT)
      }
    end
  end
end