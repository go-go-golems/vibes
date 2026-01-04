require_relative 'cqrs'
require_relative 'order_analytics_aggregate'

module AnalyticsService
  module Commands
    # Commands for order analytics
    class CalculateOrderConversionRate < CQRS::Command; end
    class GenerateDailyOrderReport < CQRS::Command; end
    class DetectInventoryAnomaly < CQRS::Command; end
    class PublishAnalyticsInsight < CQRS::Command; end
    
    # Command handlers
    class CalculateOrderConversionRateHandler < CQRS::CommandHandler
      def execute(command)
        with_aggregate(command.aggregate_id) do |aggregate|
          # In a real system, this would calculate conversion based on complex logic
          # For demo, we'll use simple random value
          conversion_rate = (70 + rand(30)) / 100.0
          
          aggregate.add_event('CONVERSION_CALCULATED', { rate: conversion_rate })
        end
      end
    end
    
    class GenerateDailyOrderReportHandler < CQRS::CommandHandler
      def initialize(repository, event_store, kafka_producer)
        super(repository)
        @event_store = event_store
        @kafka_producer = kafka_producer
      end
      
      def execute(command)
        date = command.data[:date] || Date.today.iso8601
        
        # Get all events for the day
        events = @event_store.get_all_events.select do |event|
          event_date = Time.parse(event.timestamp.to_s).to_date.iso8601
          event_date == date
        end
        
        # Calculate report metrics
        order_count = events.count { |e| e.event_type == 'ORDER_CREATED' }
        payment_success = events.count { |e| e.event_type == 'PAYMENT_PROCESSED' && e.data[:status] == 'success' }
        payment_failed = events.count { |e| e.event_type == 'PAYMENT_PROCESSED' && e.data[:status] == 'failed' }
        fulfilled = events.count { |e| e.event_type == 'ORDER_FULFILLED' }
        cancelled = events.count { |e| e.event_type == 'ORDER_CANCELLED' }
        
        # Calculate average order value
        order_values = events
          .select { |e| e.event_type == 'ORDER_CREATED' }
          .map { |e| e.data[:total_amount] }
        
        avg_order_value = order_values.empty? ? 0 : order_values.sum / order_values.size
        
        # Create report
        report = {
          date: date,
          order_count: order_count,
          payment_success: payment_success,
          payment_failed: payment_failed,
          fulfilled: fulfilled,
          cancelled: cancelled,
          avg_order_value: avg_order_value,
          generated_at: Time.now.utc.iso8601
        }
        
        # Publish report to Kafka
        if @kafka_producer
          @kafka_producer.produce_message(
            'analytics.reports.daily',
            "report-#{date}",
            report.to_json
          )
        end
        
        report
      end
    end
    
    class DetectInventoryAnomalyHandler < CQRS::CommandHandler
      def initialize(repository, event_store, kafka_producer)
        super(repository)
        @event_store = event_store
        @kafka_producer = kafka_producer
      end
      
      def execute(command)
        # Get inventory checked events
        events = @event_store.get_events_by_type('INVENTORY_CHECKED')
        
        # Group by product and calculate unavailability rate
        unavailable_products = {}
        
        events.each do |event|
          next if event.data[:all_items_available]
          
          event.data[:unavailable_items].each do |item|
            product_id = item[:product_id]
            unavailable_products[product_id] ||= { count: 0, name: item[:name] }
            unavailable_products[product_id][:count] += 1
          end
        end
        
        # Find products with high unavailability (threshold: 3 or more)
        anomalies = unavailable_products.select { |_, data| data[:count] >= 3 }
        
        # Publish anomalies to Kafka if any found
        if !anomalies.empty? && @kafka_producer
          insight = {
            type: 'inventory_anomaly',
            products: anomalies.map { |id, data| { id: id, name: data[:name], unavailable_count: data[:count] } },
            timestamp: Time.now.utc.iso8601,
            severity: anomalies.any? { |_, data| data[:count] >= 5 } ? 'high' : 'medium'
          }
          
          @kafka_producer.produce_message(
            'analytics.insights.inventory',
            "anomaly-#{Time.now.to_i}",
            insight.to_json
          )
        end
        
        { anomalies: anomalies }
      end
    end
    
    class PublishAnalyticsInsightHandler < CQRS::CommandHandler
      def initialize(repository, kafka_producer)
        super(repository)
        @kafka_producer = kafka_producer
      end
      
      def execute(command)
        return unless @kafka_producer
        
        insight_type = command.data[:type]
        insight_data = command.data[:data]
        
        # Add timestamp if not present
        insight_data[:timestamp] ||= Time.now.utc.iso8601
        
        # Publish to Kafka
        @kafka_producer.produce_message(
          "analytics.insights.#{insight_type}",
          "insight-#{Time.now.to_i}",
          insight_data.to_json
        )
        
        { published: true, type: insight_type }
      end
    end
  end
  
  module Queries
    # Queries for order analytics
    class GetOrderAnalytics < CQRS::Query; end
    class GetOrdersTimeline < CQRS::Query; end
    class GetTopProducts < CQRS::Query; end
    class GetInventoryShortages < CQRS::Query; end
    
    # Query handlers
    class GetOrderAnalyticsHandler < CQRS::QueryHandler
      def execute(query)
        order_id = query.parameters[:order_id]
        @read_model.get_order_analytics(order_id)
      end
    end
    
    class GetOrdersTimelineHandler < CQRS::QueryHandler
      def execute(query)
        days = query.parameters[:days] || 7
        @read_model.get_orders_timeline(days)
      end
    end
    
    class GetTopProductsHandler < CQRS::QueryHandler
      def execute(query)
        limit = query.parameters[:limit] || 10
        @read_model.get_top_products(limit)
      end
    end
    
    class GetInventoryShortagesHandler < CQRS::QueryHandler
      def execute(query)
        @read_model.get_unavailable_product_stats
      end
    end
  end
  
  module EventHandlers
    # Event handler for updating the read model
    class AnalyticsReadModelUpdater < CQRS::EventHandler
      def initialize(read_model)
        @read_model = read_model
      end
      
      def on_order_created(event)
        @read_model.save_order({
          order_id: event.aggregate_id,
          user_id: event.data[:user_id],
          item_count: event.data[:item_count],
          total_amount: event.data[:total_amount],
          status: 'created',
          created_at: event.timestamp
        })
      end
      
      def on_payment_processed(event)
        @read_model.save_payment({
          order_id: event.aggregate_id,
          status: event.data[:status],
          amount: event.data[:amount],
          processed_at: event.timestamp
        })
        
        if event.data[:status] == 'success'
          @read_model.update_order_status(event.aggregate_id, 'paid')
        else
          @read_model.update_order_status(event.aggregate_id, 'payment_failed')
        end
      end
      
      def on_inventory_checked(event)
        @read_model.save_inventory_check({
          order_id: event.aggregate_id,
          all_items_available: event.data[:all_items_available],
          unavailable_count: event.data[:unavailable_count],
          checked_at: event.timestamp
        })
      end
      
      def on_order_fulfilled(event)
        @read_model.save_fulfillment({
          order_id: event.aggregate_id,
          shipping_id: event.data[:shipping_id],
          fulfilled_at: event.timestamp
        })
        
        @read_model.update_order_status(event.aggregate_id, 'fulfilled')
      end
      
      def on_order_cancelled(event)
        @read_model.save_cancellation({
          order_id: event.aggregate_id,
          reason: event.data[:reason],
          cancelled_at: event.timestamp
        })
        
        @read_model.update_order_status(event.aggregate_id, 'cancelled')
      end
    end
  end
end