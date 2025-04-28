require 'prometheus/client'
require 'concurrent'

module AnalyticsService
  class Metrics
    attr_reader :registry
    
    def initialize
      @registry = Prometheus::Client.registry
      @mutex = Concurrent::ReentrantReadWriteLock.new
      
      # Basic counters
      @messages_processed = @registry.counter(
        :analytics_messages_processed_total,
        docstring: 'Total number of Kafka messages processed'
      )
      
      @topic_counters = @registry.counter(
        :analytics_messages_by_topic_total,
        docstring: 'Total number of messages processed by topic',
        labels: [:topic]
      )
      
      # Order metrics
      @orders_created = @registry.counter(
        :analytics_orders_created_total,
        docstring: 'Total number of orders created'
      )
      
      @orders_fulfilled = @registry.counter(
        :analytics_orders_fulfilled_total,
        docstring: 'Total number of orders fulfilled'
      )
      
      @orders_cancelled = @registry.counter(
        :analytics_orders_cancelled_total,
        docstring: 'Total number of orders cancelled'
      )
      
      # Payment metrics
      @successful_payments = @registry.counter(
        :analytics_payments_successful_total,
        docstring: 'Total number of successful payments'
      )
      
      @failed_payments = @registry.counter(
        :analytics_payments_failed_total,
        docstring: 'Total number of failed payments'
      )
      
      # Inventory metrics
      @inventory_available = @registry.counter(
        :analytics_inventory_available_total,
        docstring: 'Total number of inventory checks with all items available'
      )
      
      @inventory_unavailable = @registry.counter(
        :analytics_inventory_unavailable_total,
        docstring: 'Total number of inventory checks with unavailable items'
      )
      
      # Business metrics
      @order_values = @registry.histogram(
        :analytics_order_value_dollars,
        docstring: 'Distribution of order values',
        buckets: [10, 20, 50, 100, 200, 500, 1000]
      )
      
      @active_orders = @registry.gauge(
        :analytics_active_orders,
        docstring: 'Number of active (non-fulfilled, non-cancelled) orders'
      )
      
      # Service metrics
      @processing_errors = @registry.counter(
        :analytics_processing_errors_total,
        docstring: 'Total number of errors while processing events',
        labels: [:error_type]
      )
    end
    
    def increment_messages_processed
      @mutex.with_read_lock { @messages_processed.increment }
    end
    
    def increment_topic_counter(topic)
      @mutex.with_read_lock { @topic_counters.increment(labels: { topic: topic }) }
    end
    
    def increment_orders_created
      @mutex.with_read_lock do 
        @orders_created.increment
        @active_orders.increment
      end
    end
    
    def increment_orders_fulfilled
      @mutex.with_read_lock do
        @orders_fulfilled.increment
        @active_orders.decrement
      end
    end
    
    def increment_orders_cancelled
      @mutex.with_read_lock do
        @orders_cancelled.increment
        @active_orders.decrement
      end
    end
    
    def increment_successful_payments
      @mutex.with_read_lock { @successful_payments.increment }
    end
    
    def increment_failed_payments
      @mutex.with_read_lock { @failed_payments.increment }
    end
    
    def increment_inventory_available
      @mutex.with_read_lock { @inventory_available.increment }
    end
    
    def increment_inventory_unavailable
      @mutex.with_read_lock { @inventory_unavailable.increment }
    end
    
    def add_order_value(value)
      @mutex.with_read_lock { @order_values.observe(value) }
    end
    
    def increment_error(error_type)
      @mutex.with_read_lock { @processing_errors.increment(labels: { error_type: error_type }) }
    end
  end
end