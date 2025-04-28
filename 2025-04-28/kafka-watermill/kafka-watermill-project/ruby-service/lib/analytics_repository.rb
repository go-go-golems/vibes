require 'sequel'
require 'sqlite3'
require 'logger'

module AnalyticsService
  class AnalyticsRepository
    attr_reader :db, :logger
    
    def initialize(db_path, logger)
      @logger = logger
      @db = Sequel.connect("sqlite://#{db_path}")
      create_schema
    end
    
    def save_order(order_data)
      @db[:orders].insert(order_data)
    rescue => e
      @logger.error("Error saving order: #{e.message}")
      raise
    end
    
    def save_order_item(item_data)
      @db[:order_items].insert(item_data)
    rescue => e
      @logger.error("Error saving order item: #{e.message}")
      raise
    end
    
    def save_payment(payment_data)
      @db[:payments].insert(payment_data)
    rescue => e
      @logger.error("Error saving payment: #{e.message}")
      raise
    end
    
    def save_inventory_check(inventory_data)
      @db[:inventory_checks].insert(inventory_data)
    rescue => e
      @logger.error("Error saving inventory check: #{e.message}")
      raise
    end
    
    def save_unavailable_item(item_data)
      @db[:unavailable_items].insert(item_data)
    rescue => e
      @logger.error("Error saving unavailable item: #{e.message}")
      raise
    end
    
    def save_fulfillment(fulfillment_data)
      @db[:fulfillments].insert(fulfillment_data)
    rescue => e
      @logger.error("Error saving fulfillment: #{e.message}")
      raise
    end
    
    def save_cancellation(cancellation_data)
      @db[:cancellations].insert(cancellation_data)
    rescue => e
      @logger.error("Error saving cancellation: #{e.message}")
      raise
    end
    
    def update_order_status(order_id, status)
      @db[:orders].where(order_id: order_id).update(status: status)
    rescue => e
      @logger.error("Error updating order status: #{e.message}")
      raise
    end
    
    def get_summary_statistics
      {
        orders_created: @db[:orders].count,
        orders_paid: @db[:orders].where(status: 'paid').count,
        orders_fulfilled: @db[:orders].where(status: 'fulfilled').count,
        orders_cancelled: @db[:orders].where(status: 'cancelled').count,
        payments_success: @db[:payments].where(status: 'success').count,
        payments_failed: @db[:payments].where(status: 'failed').count,
        inventory_available: @db[:inventory_checks].where(all_items_available: true).count,
        inventory_unavailable: @db[:inventory_checks].where(all_items_available: false).count,
        avg_order_value: @db[:orders].avg(:total_amount) || 0
      }
    rescue => e
      @logger.error("Error getting summary statistics: #{e.message}")
      {}
    end
    
    def get_orders_by_status
      @db[:orders].group_and_count(:status).all.map { |row| [row[:status], row[:count]] }
    rescue => e
      @logger.error("Error getting orders by status: #{e.message}")
      []
    end
    
    def get_top_products(limit = 10)
      @db[:order_items]
        .select(:product_id, :product_name)
        .select_append{ sum(:quantity).as(quantity) }
        .select_append{ sum(:price * :quantity).as(revenue) }
        .group(:product_id)
        .order(Sequel.desc(:quantity))
        .limit(limit)
        .all
    rescue => e
      @logger.error("Error getting top products: #{e.message}")
      []
    end
    
    def get_orders_timeline(days = 7)
      @db[:orders]
        .where(Sequel.lit("created_at > datetime('now', '-#{days} days')"))
        .select(Sequel.lit("strftime('%Y-%m-%d', created_at) as date"))
        .select_append{ count(:order_id).as(count) }
        .group(Sequel.lit("strftime('%Y-%m-%d', created_at)"))
        .order(Sequel.asc(:date))
        .all
        .map { |row| [row[:date], row[:count]] }
    rescue => e
      @logger.error("Error getting orders timeline: #{e.message}")
      []
    end
    
    def get_unavailable_product_stats
      @db[:unavailable_items]
        .select(:product_id, :product_name)
        .select_append{ sum(:requested).as(requested) }
        .select_append{ sum(:available).as(available) }
        .select_append{ sum(:requested - :available).as(shortage) }
        .group(:product_id)
        .order(Sequel.desc(:shortage))
        .all
    rescue => e
      @logger.error("Error getting unavailable product stats: #{e.message}")
      []
    end
    
    private
    
    def create_schema
      @db.create_table?(:orders) do
        String :order_id, primary_key: true
        String :user_id
        Integer :item_count
        Float :total_amount
        String :status
        DateTime :created_at
        index :user_id
        index :status
        index :created_at
      end
      
      @db.create_table?(:order_items) do
        primary_key :id
        String :order_id
        String :product_id
        String :product_name
        Integer :quantity
        Float :price
        DateTime :timestamp
        index :order_id
        index :product_id
      end
      
      @db.create_table?(:payments) do
        String :payment_id, primary_key: true
        String :order_id
        String :status
        String :transaction_id
        Float :amount
        DateTime :processed_at
        index :order_id
        index :status
      end
      
      @db.create_table?(:inventory_checks) do
        primary_key :id
        String :order_id
        TrueClass :all_items_available
        Integer :unavailable_count
        DateTime :checked_at
        index :order_id
        index :all_items_available
      end
      
      @db.create_table?(:unavailable_items) do
        primary_key :id
        String :order_id
        String :product_id
        String :product_name
        Integer :requested
        Integer :available
        DateTime :timestamp
        index :order_id
        index :product_id
      end
      
      @db.create_table?(:fulfillments) do
        String :order_id, primary_key: true
        String :shipping_id
        String :tracking_number
        String :status
        DateTime :fulfilled_at
        index :status
      end
      
      @db.create_table?(:cancellations) do
        String :order_id, primary_key: true
        String :reason
        String :refund_status
        DateTime :cancelled_at
        index :reason
      end
    end
  end
end