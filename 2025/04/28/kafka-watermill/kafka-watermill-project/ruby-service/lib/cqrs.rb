require_relative 'event_sourcing'

module AnalyticsService
  module CQRS
    # Command base class
    class Command
      attr_reader :command_id, :aggregate_id, :data

      def initialize(aggregate_id, data = {})
        @command_id = SecureRandom.uuid
        @aggregate_id = aggregate_id
        @data = data
      end

      def to_h
        {
          command_id: @command_id,
          aggregate_id: @aggregate_id,
          command_type: self.class.name.split('::').last,
          data: @data
        }
      end
    end

    # Command handler
    class CommandHandler
      def initialize(repository)
        @repository = repository
      end
      
      def execute(command)
        # Default implementation to be overridden by concrete handlers
        raise NotImplementedError, "#{self.class} has not implemented method '#{__method__}'"
      end
      
      protected
      
      def with_aggregate(aggregate_id)
        aggregate = @repository.load(aggregate_id)
        yield(aggregate)
        @repository.save(aggregate)
        aggregate
      end
    end

    # Query base class
    class Query
      attr_reader :query_id, :parameters

      def initialize(parameters = {})
        @query_id = SecureRandom.uuid
        @parameters = parameters
      end
    end

    # Query handler
    class QueryHandler
      def initialize(read_model)
        @read_model = read_model
      end
      
      def execute(query)
        # Default implementation to be overridden by concrete handlers
        raise NotImplementedError, "#{self.class} has not implemented method '#{__method__}'"
      end
    end

    # Command bus
    class CommandBus
      def initialize
        @handlers = {}
      end
      
      def register(command_class, handler)
        @handlers[command_class.name] = handler
      end
      
      def dispatch(command)
        handler = @handlers[command.class.name]
        if handler.nil?
          raise "No handler registered for command #{command.class.name}"
        end
        
        handler.execute(command)
      end
    end

    # Query bus
    class QueryBus
      def initialize
        @handlers = {}
      end
      
      def register(query_class, handler)
        @handlers[query_class.name] = handler
      end
      
      def dispatch(query)
        handler = @handlers[query.class.name]
        if handler.nil?
          raise "No handler registered for query #{query.class.name}"
        end
        
        handler.execute(query)
      end
    end
    
    # Event bus for event notifications
    class EventBus
      def initialize
        @handlers = {}
      end
      
      def register(event_type, handler)
        @handlers[event_type] ||= []
        @handlers[event_type] << handler
      end
      
      def publish(event)
        handlers = @handlers[event.event_type] || []
        handlers.each do |handler|
          begin
            handler.handle(event)
          rescue => e
            # Log error but continue processing other handlers
            puts "Error handling event #{event.event_type}: #{e.message}"
          end
        end
      end
    end
    
    # Event handler
    class EventHandler
      def handle(event)
        method_name = "on_#{event.event_type.downcase}"
        if respond_to?(method_name, true)
          send(method_name, event)
        else
          # Default handler
          on_event(event)
        end
      end
      
      protected
      
      def on_event(event)
        # Default implementation to be overridden by concrete handlers
      end
    end
  end
end