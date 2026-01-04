# AUTO-GENERATED — DO NOT EDIT MANUALLY
# Protoc version: 3.21.12
# Source file: order.proto

require 'json'
require 'time'

module KafkaWatermill
  module IDL
    # Base class for all events
    class Event
      def to_json(*_args)
        JSON.generate(to_h)
      end

      def self.from_json(json_str)
        new(**JSON.parse(json_str, symbolize_names: true))
      end
    end

    # Supporting classes

    class OrderItem
      attr_reader :product_id, :name, :quantity, :price

      def initialize(product_id:, name:, quantity:, price:)
        @product_id = product_id
        @name = name
        @quantity = quantity
        @price = price
      end

      def to_h
        {
          product_id: @product_id,
          name: @name,
          quantity: @quantity,
          price: @price
        }
      end
    end

    class UnavailableItem
      attr_reader :product_id, :name, :requested_quantity, :available_quantity

      def initialize(product_id:, name:, requested_quantity:, available_quantity:)
        @product_id = product_id
        @name = name
        @requested_quantity = requested_quantity
        @available_quantity = available_quantity
      end

      def to_h
        {
          product_id: @product_id,
          name: @name,
          requested_quantity: @requested_quantity,
          available_quantity: @available_quantity
        }
      end
    end

    # Event classes

    class OrderCreated < Event
      attr_reader :order_id, :user_id, :items, :total_amount, :timestamp

      def initialize(order_id:, user_id:, items:, total_amount:, timestamp:)
        @order_id = order_id
        @user_id = user_id
        @items = items.map { |item| item.is_a?(Hash) ? Orderitem.new(**item) : item }
        @total_amount = total_amount
        @timestamp = timestamp.is_a?(String) ? Time.parse(timestamp) : timestamp
      end

      def to_h
        {
          order_id: @order_id,
          user_id: @user_id,
          items: @items.map(&:to_h),
          total_amount: @total_amount,
          timestamp: @timestamp.iso8601
        }
      end
    end

    class PaymentProcessed < Event
      attr_reader :order_id, :payment_id, :status, :transaction_id, :amount, :timestamp

      def initialize(order_id:, payment_id:, status:, transaction_id:, amount:, timestamp:)
        @order_id = order_id
        @payment_id = payment_id
        @status = status
        @transaction_id = transaction_id
        @amount = amount
        @timestamp = timestamp.is_a?(String) ? Time.parse(timestamp) : timestamp
      end

      def to_h
        {
          order_id: @order_id,
          payment_id: @payment_id,
          status: @status,
          transaction_id: @transaction_id,
          amount: @amount,
          timestamp: @timestamp.iso8601
        }
      end
    end

    class InventoryChecked < Event
      attr_reader :order_id, :all_items_available, :unavailable_items, :timestamp

      def initialize(order_id:, all_items_available:, unavailable_items:, timestamp:)
        @order_id = order_id
        @all_items_available = all_items_available
        @unavailable_items = unavailable_items.map { |item| item.is_a?(Hash) ? Unavailableitem.new(**item) : item }
        @timestamp = timestamp.is_a?(String) ? Time.parse(timestamp) : timestamp
      end

      def to_h
        {
          order_id: @order_id,
          all_items_available: @all_items_available,
          unavailable_items: @unavailable_items.map(&:to_h),
          timestamp: @timestamp.iso8601
        }
      end
    end

    class OrderFulfilled < Event
      attr_reader :order_id, :shipping_id, :tracking_number, :status, :timestamp

      def initialize(order_id:, shipping_id:, tracking_number:, status:, timestamp:)
        @order_id = order_id
        @shipping_id = shipping_id
        @tracking_number = tracking_number
        @status = status
        @timestamp = timestamp.is_a?(String) ? Time.parse(timestamp) : timestamp
      end

      def to_h
        {
          order_id: @order_id,
          shipping_id: @shipping_id,
          tracking_number: @tracking_number,
          status: @status,
          timestamp: @timestamp.iso8601
        }
      end
    end

    class OrderCancelled < Event
      attr_reader :order_id, :reason, :refund_status, :timestamp

      def initialize(order_id:, reason:, refund_status:, timestamp:)
        @order_id = order_id
        @reason = reason
        @refund_status = refund_status
        @timestamp = timestamp.is_a?(String) ? Time.parse(timestamp) : timestamp
      end

      def to_h
        {
          order_id: @order_id,
          reason: @reason,
          refund_status: @refund_status,
          timestamp: @timestamp.iso8601
        }
      end
    end

    class Notification < Event
      attr_reader :order_id, :notification_type, :recipient, :content, :timestamp

      def initialize(order_id:, notification_type:, recipient:, content:, timestamp:)
        @order_id = order_id
        @notification_type = notification_type
        @recipient = recipient
        @content = content
        @timestamp = timestamp.is_a?(String) ? Time.parse(timestamp) : timestamp
      end

      def to_h
        {
          order_id: @order_id,
          notification_type: @notification_type,
          recipient: @recipient,
          content: @content,
          timestamp: @timestamp.iso8601
        }
      end
    end

    class SagaEvent < Event
      attr_reader :saga_id, :saga_name, :status, :timestamp, :error

      def initialize(saga_id:, saga_name:, status:, timestamp:, error:)
        @saga_id = saga_id
        @saga_name = saga_name
        @status = status
        @timestamp = timestamp.is_a?(String) ? Time.parse(timestamp) : timestamp
        @error = error
      end

      def to_h
        {
          saga_id: @saga_id,
          saga_name: @saga_name,
          status: @status,
          timestamp: @timestamp.iso8601,
          error: @error
        }
      end
    end
  end
end
