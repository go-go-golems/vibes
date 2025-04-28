#!/usr/bin/env ruby

require 'logger'
require 'concurrent'
require_relative 'lib/kafka_client'
require_relative 'lib/event_processor'
require_relative 'lib/analytics_repository'
require_relative 'lib/metrics'
require_relative 'lib/analytics_web'
require_relative 'lib/event_sourcing'
require_relative 'lib/cqrs'
require_relative 'lib/order_analytics_aggregate'
require_relative 'lib/analytics_commands'

module AnalyticsService
  class Application
    attr_reader :logger, :kafka_client, :event_processor, :analytics_repo, :metrics,
                :event_store, :command_bus, :query_bus, :event_bus

    def initialize
      @logger = Logger.new(STDOUT)
      @logger.level = ENV['LOG_LEVEL'] ? Logger.const_get(ENV['LOG_LEVEL']) : Logger::INFO

      @logger.info('Initializing Analytics Service...')

      # Initialize components
      db_path = ENV['DB_PATH'] || 'db/analytics.db'
      @db = Sequel.connect("sqlite://#{db_path}")
      @metrics = Metrics.new
      @analytics_repo = AnalyticsRepository.new(db_path, @logger)

      kafka_config = {
        brokers: ENV['KAFKA_BROKERS']&.split(',') || ['kafka:9092'],
        client_id: ENV['KAFKA_CLIENT_ID'] || 'analytics-service',
        consumer_group: ENV['KAFKA_CONSUMER_GROUP'] || 'analytics-service-group',
        logger: @logger
      }

      @kafka_client = KafkaClient.new(kafka_config)

      # Initialize event sourcing components
      @event_store = EventSourcing::EventStore.new(@db)
      @repository = EventSourcing::Repository.new(@event_store, OrderAnalyticsAggregate)

      # Initialize CQRS components
      @command_bus = CQRS::CommandBus.new
      @query_bus = CQRS::QueryBus.new
      @event_bus = CQRS::EventBus.new

      # Register command handlers
      @command_bus.register(
        Commands::CalculateOrderConversionRate,
        Commands::CalculateOrderConversionRateHandler.new(@repository)
      )

      @command_bus.register(
        Commands::GenerateDailyOrderReport,
        Commands::GenerateDailyOrderReportHandler.new(@repository, @event_store, @kafka_client)
      )

      @command_bus.register(
        Commands::DetectInventoryAnomaly,
        Commands::DetectInventoryAnomalyHandler.new(@repository, @event_store, @kafka_client)
      )

      @command_bus.register(
        Commands::PublishAnalyticsInsight,
        Commands::PublishAnalyticsInsightHandler.new(@repository, @kafka_client)
      )

      # Register query handlers
      @query_bus.register(
        Queries::GetOrderAnalytics,
        Queries::GetOrderAnalyticsHandler.new(@analytics_repo)
      )

      @query_bus.register(
        Queries::GetOrdersTimeline,
        Queries::GetOrdersTimelineHandler.new(@analytics_repo)
      )

      @query_bus.register(
        Queries::GetTopProducts,
        Queries::GetTopProductsHandler.new(@analytics_repo)
      )

      @query_bus.register(
        Queries::GetInventoryShortages,
        Queries::GetInventoryShortagesHandler.new(@analytics_repo)
      )

      # Register event handlers
      @event_bus.register(
        'ORDER_CREATED',
        EventHandlers::AnalyticsReadModelUpdater.new(@analytics_repo)
      )

      @event_bus.register(
        'PAYMENT_PROCESSED',
        EventHandlers::AnalyticsReadModelUpdater.new(@analytics_repo)
      )

      @event_bus.register(
        'INVENTORY_CHECKED',
        EventHandlers::AnalyticsReadModelUpdater.new(@analytics_repo)
      )

      @event_bus.register(
        'ORDER_FULFILLED',
        EventHandlers::AnalyticsReadModelUpdater.new(@analytics_repo)
      )

      @event_bus.register(
        'ORDER_CANCELLED',
        EventHandlers::AnalyticsReadModelUpdater.new(@analytics_repo)
      )

      # Initialize event processor with the event bus
      @event_processor = EventProcessor.new(@logger, @analytics_repo, @metrics)

      # Configure AnalyticsWeb settings instead of creating an instance
      AnalyticsWeb.set :analytics_repo, @analytics_repo
      AnalyticsWeb.set :metrics, @metrics
      AnalyticsWeb.set :kafka_producer, @kafka_client # Pass the Kafka client as the producer
    end

    def start
      @logger.info('Starting Analytics Service...')

      # Start Kafka consumer in a background thread
      consumer_thread = @kafka_client.start_consumer(@event_processor)

      # Set up periodic tasks
      setup_periodic_tasks

      # Start web server in the main thread
      web_thread = start_web_server

      # Handle shutdown gracefully
      setup_shutdown_handler

      # Wait for processes to complete (they shouldn't unless there's an error)
      consumer_thread.wait
      web_thread.join if web_thread.is_a?(Thread)

      @logger.info('Analytics Service stopped.')
    end

    private

    def setup_periodic_tasks
      # Generate daily report every 24 hours
      Concurrent::TimerTask.new(
        execution_interval: 24 * 60 * 60, # 24 hours
        timeout_interval: 5 * 60          # 5 minutes timeout
      ) do
        @logger.info('Generating daily order report...')
        begin
          @command_bus.dispatch(Commands::GenerateDailyOrderReport.new(nil, {}))
          @logger.info('Daily report generated successfully')
        rescue StandardError => e
          @logger.error("Error generating daily report: #{e.message}")
          @logger.error(e.backtrace.join("\n"))
        end
      end.execute

      # Detect inventory anomalies every hour
      Concurrent::TimerTask.new(
        execution_interval: 60 * 60,      # 1 hour
        timeout_interval: 5 * 60          # 5 minutes timeout
      ) do
        @logger.info('Detecting inventory anomalies...')
        begin
          @command_bus.dispatch(Commands::DetectInventoryAnomaly.new(nil, {}))
          @logger.info('Inventory anomaly detection completed')
        rescue StandardError => e
          @logger.error("Error detecting inventory anomalies: #{e.message}")
          @logger.error(e.backtrace.join("\n"))
        end
      end.execute
    end

    def start_web_server
      Thread.new do
        @logger.info("Starting web server on port #{AnalyticsWeb.settings.port}...")
        AnalyticsWeb.run!
      end
    end

    def setup_shutdown_handler
      %w[INT TERM].each do |signal|
        trap(signal) do
          @logger.info("Received signal #{signal}, shutting down...")
          @kafka_client.stop_consumer
          @logger.info('Shutdown complete.')
          exit
        end
      end
    end
  end
end

# Start the application if this file is run directly
if __FILE__ == $0
  app = AnalyticsService::Application.new
  app.start
end
