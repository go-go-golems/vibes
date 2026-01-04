require 'json'
require 'time'
require 'securerandom'

module AnalyticsService
  module EventSourcing
    # Base class for all events
    class Event
      attr_reader :event_id, :aggregate_id, :event_type, :timestamp, :data

      def initialize(aggregate_id, event_type, data = {})
        @event_id = SecureRandom.uuid
        @aggregate_id = aggregate_id
        @event_type = event_type
        @timestamp = Time.now.utc
        @data = data
      end

      def to_h
        {
          event_id: @event_id,
          aggregate_id: @aggregate_id,
          event_type: @event_type,
          timestamp: @timestamp.iso8601,
          data: @data
        }
      end

      def to_json
        JSON.generate(to_h)
      end

      def self.from_h(hash)
        event = self.new(hash[:aggregate_id], hash[:event_type], hash[:data])
        event.instance_variable_set(:@event_id, hash[:event_id])
        event.instance_variable_set(:@timestamp, Time.parse(hash[:timestamp]))
        event
      end

      def self.from_json(json_str)
        from_h(JSON.parse(json_str, symbolize_names: true))
      end
    end

    # Base class for all aggregates
    class Aggregate
      attr_reader :id, :version

      def initialize(id)
        @id = id
        @version = 0
        @uncommitted_events = []
      end

      def apply_event(event)
        method_name = "apply_#{event.event_type.downcase}"
        if respond_to?(method_name, true)
          send(method_name, event.data)
          @version += 1
        else
          raise "No event handler found for #{event.event_type}"
        end
      end

      def apply_all_events(events)
        events.each { |event| apply_event(event) }
      end

      def add_event(event_type, data = {})
        event = Event.new(@id, event_type, data)
        apply_event(event)
        @uncommitted_events << event
        event
      end

      def get_uncommitted_events
        @uncommitted_events
      end

      def clear_uncommitted_events
        @uncommitted_events = []
      end
    end

    # Repository for storing and retrieving aggregates
    class Repository
      def initialize(event_store, aggregate_class)
        @event_store = event_store
        @aggregate_class = aggregate_class
      end

      def save(aggregate)
        # Save all uncommitted events to the event store
        @event_store.save_events(aggregate.get_uncommitted_events)
        aggregate.clear_uncommitted_events
      end

      def load(aggregate_id)
        # Create a new aggregate instance
        aggregate = @aggregate_class.new(aggregate_id)
        
        # Load all events for this aggregate
        events = @event_store.get_events_for_aggregate(aggregate_id)
        
        # Apply all events to the aggregate
        aggregate.apply_all_events(events)
        
        aggregate
      end
    end

    # Store for persisting events
    class EventStore
      def initialize(db)
        @db = db
        ensure_event_store_table
      end

      def save_events(events)
        @db.transaction do
          events.each do |event|
            @db[:events].insert(
              event_id: event.event_id,
              aggregate_id: event.aggregate_id,
              event_type: event.event_type,
              timestamp: event.timestamp,
              data: JSON.generate(event.data)
            )
          end
        end
      end

      def get_events_for_aggregate(aggregate_id)
        @db[:events]
          .where(aggregate_id: aggregate_id)
          .order(:timestamp)
          .map { |row| Event.from_h(row_to_event_hash(row)) }
      end

      def get_all_events
        @db[:events]
          .order(:timestamp)
          .map { |row| Event.from_h(row_to_event_hash(row)) }
      end

      def get_events_by_type(event_type)
        @db[:events]
          .where(event_type: event_type)
          .order(:timestamp)
          .map { |row| Event.from_h(row_to_event_hash(row)) }
      end

      private

      def row_to_event_hash(row)
        {
          event_id: row[:event_id],
          aggregate_id: row[:aggregate_id],
          event_type: row[:event_type],
          timestamp: row[:timestamp].to_s,
          data: JSON.parse(row[:data], symbolize_names: true)
        }
      end

      def ensure_event_store_table
        @db.create_table?(:events) do
          String :event_id, primary_key: true
          String :aggregate_id
          String :event_type
          DateTime :timestamp
          String :data, text: true
          
          index :aggregate_id
          index :event_type
          index :timestamp
        end
      end
    end
  end
end