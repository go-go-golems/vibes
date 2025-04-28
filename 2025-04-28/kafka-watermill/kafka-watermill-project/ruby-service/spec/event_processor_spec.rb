require 'rspec'
require 'logger'
require_relative '../lib/event_processor'
require_relative '../../idl/ruby/order_events'

describe AnalyticsService::EventProcessor do
  let(:logger) { instance_double(Logger, info: nil, debug: nil, error: nil) }
  let(:analytics_repo) { double('AnalyticsRepository') }
  let(:metrics) { double('Metrics') }
  let(:processor) { AnalyticsService::EventProcessor.new(logger, analytics_repo, metrics) }
  
  before do
    allow(metrics).to receive(:increment_messages_processed)
    allow(metrics).to receive(:increment_topic_counter)
    allow(metrics).to receive(:increment_orders_created)
    allow(metrics).to receive(:increment_successful_payments)
    allow(metrics).to receive(:increment_failed_payments)
    allow(metrics).to receive(:increment_inventory_available)
    allow(metrics).to receive(:increment_inventory_unavailable)
    allow(metrics).to receive(:increment_orders_fulfilled)
    allow(metrics).to receive(:increment_orders_cancelled)
    allow(metrics).to receive(:add_order_value)
    
    allow(analytics_repo).to receive(:save_order)
    allow(analytics_repo).to receive(:save_order_item)
    allow(analytics_repo).to receive(:save_payment)
    allow(analytics_repo).to receive(:save_inventory_check)
    allow(analytics_repo).to receive(:save_unavailable_item)
    allow(analytics_repo).to receive(:save_fulfillment)
    allow(analytics_repo).to receive(:save_cancellation)
    allow(analytics_repo).to receive(:update_order_status)
    allow(analytics_repo).to receive(:get_summary_statistics).and_return({})
  end
  
  describe '#process_message' do
    context 'with order.created message' do
      let(:order_created) do
        KafkaWatermill::IDL::OrderCreated.new(
          order_id: 'order-123',
          user_id: 'user-456',
          items: [
            { product_id: 'prod-1', name: 'Product 1', quantity: 2, price: 10.99 }
          ],
          total_amount: 21.98,
          timestamp: Time.now
        )
      end
      
      let(:message) do
        double('Message', 
          topic: 'order.created',
          partition: 0,
          offset: 1,
          value: order_created.to_json
        )
      end
      
      it 'processes order created event correctly' do
        expect(analytics_repo).to receive(:save_order).with(hash_including(
          order_id: 'order-123',
          user_id: 'user-456',
          total_amount: 21.98
        ))
        
        expect(analytics_repo).to receive(:save_order_item).with(hash_including(
          order_id: 'order-123',
          product_id: 'prod-1',
          quantity: 2,
          price: 10.99
        ))
        
        expect(metrics).to receive(:increment_orders_created)
        expect(metrics).to receive(:add_order_value).with(21.98)
        
        processor.process_message(message)
      end
    end
    
    context 'with payment.processed message' do
      let(:payment_processed) do
        KafkaWatermill::IDL::PaymentProcessed.new(
          order_id: 'order-123',
          payment_id: 'payment-789',
          status: 'success',
          transaction_id: 'trans-abc',
          amount: 21.98,
          timestamp: Time.now
        )
      end
      
      let(:message) do
        double('Message', 
          topic: 'payment.processed',
          partition: 0,
          offset: 2,
          value: payment_processed.to_json
        )
      end
      
      it 'processes payment processed event correctly' do
        expect(analytics_repo).to receive(:save_payment).with(hash_including(
          order_id: 'order-123',
          payment_id: 'payment-789',
          status: 'success',
          amount: 21.98
        ))
        
        expect(analytics_repo).to receive(:update_order_status).with('order-123', 'paid')
        expect(metrics).to receive(:increment_successful_payments)
        
        processor.process_message(message)
      end
    end
  end
end