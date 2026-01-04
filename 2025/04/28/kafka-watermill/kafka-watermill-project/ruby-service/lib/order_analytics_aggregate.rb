require_relative 'event_sourcing'

module AnalyticsService
  # Order analytics aggregate
  class OrderAnalyticsAggregate < EventSourcing::Aggregate
    attr_reader :created_at, :user_id, :item_count, :order_value, 
                :payment_status, :inventory_status, :fulfillment_status, 
                :processing_time, :conversion_rate

    def initialize(id)
      super(id)
      @user_id = nil
      @created_at = nil
      @item_count = 0
      @order_value = 0.0
      @payment_status = nil
      @inventory_status = nil
      @fulfillment_status = nil
      @processing_time = nil
      @conversion_rate = nil
    end

    # Event handlers
    private

    def apply_order_created(data)
      @created_at = Time.parse(data[:timestamp])
      @user_id = data[:user_id]
      @item_count = data[:item_count]
      @order_value = data[:total_amount]
    end

    def apply_payment_processed(data)
      @payment_status = data[:status]
      
      # Calculate time from order creation to payment
      if @created_at && data[:timestamp]
        payment_time = Time.parse(data[:timestamp])
        @processing_time ||= {}
        @processing_time[:payment] = (payment_time - @created_at).to_i
      end
    end

    def apply_inventory_checked(data)
      @inventory_status = data[:all_items_available] ? 'available' : 'unavailable'
      
      # Calculate time from order creation to inventory check
      if @created_at && data[:timestamp]
        inventory_time = Time.parse(data[:timestamp])
        @processing_time ||= {}
        @processing_time[:inventory] = (inventory_time - @created_at).to_i
      end
    end

    def apply_order_fulfilled(data)
      @fulfillment_status = 'fulfilled'
      
      # Calculate time from order creation to fulfillment
      if @created_at && data[:timestamp]
        fulfillment_time = Time.parse(data[:timestamp])
        @processing_time ||= {}
        @processing_time[:fulfillment] = (fulfillment_time - @created_at).to_i
        @processing_time[:total] = (fulfillment_time - @created_at).to_i
      end
    end

    def apply_order_cancelled(data)
      @fulfillment_status = 'cancelled'
      
      # Calculate time from order creation to cancellation
      if @created_at && data[:timestamp]
        cancellation_time = Time.parse(data[:timestamp])
        @processing_time ||= {}
        @processing_time[:cancellation] = (cancellation_time - @created_at).to_i
        @processing_time[:total] = (cancellation_time - @created_at).to_i
      end
    end

    def apply_conversion_calculated(data)
      @conversion_rate = data[:rate]
    end
  end

  # Factory for creating Order Analytics events
  class OrderAnalyticsEventFactory
    def self.create_order_event(order_id, user_id, items, total_amount, timestamp)
      EventSourcing::Event.new(
        order_id,
        'ORDER_CREATED',
        {
          user_id: user_id,
          item_count: items.size,
          total_amount: total_amount,
          timestamp: timestamp
        }
      )
    end

    def self.create_payment_event(order_id, status, amount, timestamp)
      EventSourcing::Event.new(
        order_id,
        'PAYMENT_PROCESSED',
        {
          status: status,
          amount: amount,
          timestamp: timestamp
        }
      )
    end

    def self.create_inventory_event(order_id, all_items_available, unavailable_count, timestamp)
      EventSourcing::Event.new(
        order_id,
        'INVENTORY_CHECKED',
        {
          all_items_available: all_items_available,
          unavailable_count: unavailable_count,
          timestamp: timestamp
        }
      )
    end

    def self.create_fulfillment_event(order_id, shipping_id, timestamp)
      EventSourcing::Event.new(
        order_id,
        'ORDER_FULFILLED',
        {
          shipping_id: shipping_id,
          timestamp: timestamp
        }
      )
    end

    def self.create_cancellation_event(order_id, reason, timestamp)
      EventSourcing::Event.new(
        order_id,
        'ORDER_CANCELLED',
        {
          reason: reason,
          timestamp: timestamp
        }
      )
    end
  end
end