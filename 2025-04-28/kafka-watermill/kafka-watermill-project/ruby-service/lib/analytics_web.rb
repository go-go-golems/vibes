require 'sinatra/base'
require 'sinatra/json'
require 'sinatra/reloader'
require 'prometheus/client/formats/text'
require 'chartkick'
require 'json'

module AnalyticsService
  class AnalyticsWeb < Sinatra::Base
    configure :development do
      register Sinatra::Reloader
    end

    set :views, File.expand_path('../views', __dir__)
    set :public_folder, File.expand_path('../public', __dir__)
    set :bind, '0.0.0.0'
    set :port, 3000

    # Home page with dashboard
    get '/' do
      @summary = settings.analytics_repo.get_summary_statistics
      @orders_by_status = settings.analytics_repo.get_orders_by_status
      @top_products = settings.analytics_repo.get_top_products(5)
      @orders_timeline = settings.analytics_repo.get_orders_timeline
      @unavailable_products = settings.analytics_repo.get_unavailable_product_stats
      erb :dashboard
    end

    # API endpoints
    get '/api/analytics/summary' do
      json settings.analytics_repo.get_summary_statistics
    end

    get '/api/analytics/orders/status' do
      json settings.analytics_repo.get_orders_by_status
    end

    get '/api/analytics/orders/timeline' do
      days = params[:days] ? params[:days].to_i : 7
      json settings.analytics_repo.get_orders_timeline(days)
    end

    get '/api/analytics/products/top' do
      limit = params[:limit] ? params[:limit].to_i : 10
      json settings.analytics_repo.get_top_products(limit)
    end

    get '/api/analytics/products/unavailable' do
      json settings.analytics_repo.get_unavailable_product_stats
    end

    # Publish analytical insights to Kafka
    post '/api/insights/publish' do
      halt 400, json(error: 'No Kafka producer available') unless settings.kafka_producer

      # Get data from request
      data = JSON.parse(request.body.read)
      insight_type = data['type']
      insight_payload = data['payload']

      # Add timestamp
      insight_payload['timestamp'] = Time.now.iso8601

      # Publish to Kafka
      topic = "analytics.insights.#{insight_type}"
      settings.kafka_producer.produce_message(
        topic,
        "insight-#{Time.now.to_i}",
        insight_payload.to_json
      )

      json success: true, message: "Published insight to #{topic}"
    end

    # Prometheus metrics endpoint
    get '/metrics' do
      content_type 'text/plain'
      Prometheus::Client::Formats::Text.marshal(settings.metrics.registry)
    end

    # Health check endpoint
    get '/health' do
      json status: 'UP', timestamp: Time.now.iso8601
    end
  end
end
