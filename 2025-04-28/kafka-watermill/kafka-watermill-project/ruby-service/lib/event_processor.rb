require_relative '../../idl/ruby/order_events'
require_relative 'analytics_repository'
require 'descriptive_statistics'
require 'json'

module AnalyticsService
  class EventProcessor
    attr_reader :logger, :analytics_repo, :metrics

    def initialize(logger, analytics_repo, metrics)
      @logger = logger
      @analytics_repo = analytics_repo
      @metrics = metrics
      
      # Initialize counters
      @processed_count = 0
    end

    def process_message(message)
      topic = message.topic
      value = message.value
      
      # Increment message processed counter
      @processed_count += 1
      
      # Log processing information
      @logger.debug("Processing message from topic: #{topic}, partition: #{message.partition}, offset: #{message.offset}")
      
      # Update metrics
      @metrics.increment_messages_processed
      @metrics.increment_topic_counter(topic)
      
      # Process event based on topic
      case topic
      when 'order.created'
        process_order_created(value)
      when 'payment.processed'
        process_payment_processed(value)
      when 'inventory.checked'
        process_inventory_checked(value)
      when 'order.fulfilled'
        process_order_fulfilled(value)
      when 'order.cancelled'
        process_order_cancelled(value)
      else
        @logger.warn("Received message from unhandled topic: #{topic}")
      end
      
      # Periodically log stats
      if (@processed_count % 10) == 0
        log_stats
      end
    end

    private

    def process_order_created(payload)
      begin
        event = KafkaWatermill::IDL::OrderCreated.from_json(payload)
        @logger.info("Order created: #{event.order_id} by user: #{event.user_id}, amount: #{event.total_amount}")
        
        # Store order data in repository
        @analytics_repo.save_order({
          order_id: event.order_id,
          user_id: event.user_id,
          item_count: event.items.size,
          total_amount: event.total_amount,
          status: 'created',
          created_at: event.timestamp
        })
        
        # Update metrics
        @metrics.increment_orders_created
        @metrics.add_order_value(event.total_amount)
        
        # Process individual items for product analytics
        event.items.each do |item|
          @analytics_repo.save_order_item({
            order_id: event.order_id,
            product_id: item.product_id,
            product_name: item.name,
            quantity: item.quantity,
            price: item.price,
            timestamp: event.timestamp
          })
        end
      rescue => e
        @logger.error("Error processing order.created event: #{e.message}")
        raise
      end
    end

    def process_payment_processed(payload)
      begin
        event = KafkaWatermill::IDL::PaymentProcessed.from_json(payload)
        @logger.info("Payment processed: #{event.payment_id} for order: #{event.order_id}, status: #{event.status}")
        
        # Store payment data in repository
        @analytics_repo.save_payment({
          order_id: event.order_id,
          payment_id: event.payment_id,
          status: event.status,
          transaction_id: event.transaction_id,
          amount: event.amount,
          processed_at: event.timestamp
        })
        
        # Update metrics based on payment status
        if event.status == 'success'
          @metrics.increment_successful_payments
          @analytics_repo.update_order_status(event.order_id, 'paid')
        elsif event.status == 'failed'
          @metrics.increment_failed_payments
          @analytics_repo.update_order_status(event.order_id, 'payment_failed')
        end
      rescue => e
        @logger.error("Error processing payment.processed event: #{e.message}")
        raise
      end
    end

    def process_inventory_checked(payload)
      begin
        event = KafkaWatermill::IDL::InventoryChecked.from_json(payload)
        @logger.info("Inventory checked for order: #{event.order_id}, all available: #{event.all_items_available}")
        
        # Store inventory check data
        @analytics_repo.save_inventory_check({
          order_id: event.order_id,
          all_items_available: event.all_items_available,
          unavailable_count: event.unavailable_items.size,
          checked_at: event.timestamp
        })
        
        # Update metrics
        if event.all_items_available
          @metrics.increment_inventory_available
        else
          @metrics.increment_inventory_unavailable
          
          # Record unavailable items for inventory analysis
          event.unavailable_items.each do |item|
            @analytics_repo.save_unavailable_item({
              order_id: event.order_id,
              product_id: item.product_id,
              product_name: item.name,
              requested: item.requested_quantity,
              available: item.available_quantity,
              timestamp: event.timestamp
            })
          end
        end
      rescue => e
        @logger.error("Error processing inventory.checked event: #{e.message}")
        raise
      end
    end

    def process_order_fulfilled(payload)
      begin
        event = KafkaWatermill::IDL::OrderFulfilled.from_json(payload)
        @logger.info("Order fulfilled: #{event.order_id}, shipping ID: #{event.shipping_id}")
        
        # Store fulfillment data
        @analytics_repo.save_fulfillment({
          order_id: event.order_id,
          shipping_id: event.shipping_id,
          tracking_number: event.tracking_number,
          status: event.status,
          fulfilled_at: event.timestamp
        })
        
        # Update order status
        @analytics_repo.update_order_status(event.order_id, 'fulfilled')
        
        # Update metrics
        @metrics.increment_orders_fulfilled
      rescue => e
        @logger.error("Error processing order.fulfilled event: #{e.message}")
        raise
      end
    end

    def process_order_cancelled(payload)
      begin
        event = KafkaWatermill::IDL::OrderCancelled.from_json(payload)
        @logger.info("Order cancelled: #{event.order_id}, reason: #{event.reason}")
        
        # Store cancellation data
        @analytics_repo.save_cancellation({
          order_id: event.order_id,
          reason: event.reason,
          refund_status: event.refund_status,
          cancelled_at: event.timestamp
        })
        
        # Update order status
        @analytics_repo.update_order_status(event.order_id, 'cancelled')
        
        # Update metrics
        @metrics.increment_orders_cancelled
      rescue => e
        @logger.error("Error processing order.cancelled event: #{e.message}")
        raise
      end
    end

    def log_stats
      stats = @analytics_repo.get_summary_statistics
      @logger.info("ANALYTICS REPORT:")
      @logger.info("  Orders: Created=#{stats[:orders_created]}, Paid=#{stats[:orders_paid]}, Fulfilled=#{stats[:orders_fulfilled]}, Cancelled=#{stats[:orders_cancelled]}")
      @logger.info("  Payments: Success=#{stats[:payments_success]}, Failed=#{stats[:payments_failed]}")
      @logger.info("  Inventory: Available=#{stats[:inventory_available]}, Unavailable=#{stats[:inventory_unavailable]}")
      @logger.info("  Average Order Value: $#{sprintf('%.2f', stats[:avg_order_value])}")
    end
  end
end