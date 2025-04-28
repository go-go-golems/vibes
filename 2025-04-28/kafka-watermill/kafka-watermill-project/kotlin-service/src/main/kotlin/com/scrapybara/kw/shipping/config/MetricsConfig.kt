package com.scrapybara.kw.shipping.config

import io.micrometer.core.instrument.MeterRegistry
import io.micrometer.core.instrument.Counter
import io.micrometer.core.instrument.Timer
import org.springframework.context.annotation.Bean
import org.springframework.context.annotation.Configuration
import org.springframework.stereotype.Component
import java.util.concurrent.atomic.AtomicInteger

@Configuration
class MetricsConfig {
    @Bean
    fun shippingMetrics(registry: MeterRegistry): ShippingMetrics {
        return ShippingMetrics(registry)
    }
}

@Component
class ShippingMetrics(private val registry: MeterRegistry) {
    // Counters
    val ordersProcessed: Counter = Counter.builder("shipping.orders.processed")
        .description("Number of orders processed by the shipping service")
        .register(registry)
    
    val ordersFulfilled: Counter = Counter.builder("shipping.orders.fulfilled")
        .description("Number of orders fulfilled successfully")
        .register(registry)
    
    val ordersCancelled: Counter = Counter.builder("shipping.orders.cancelled")
        .description("Number of orders cancelled due to inventory issues")
        .register(registry)
    
    val kafkaMessagesReceived: Counter = Counter.builder("shipping.kafka.messages.received")
        .description("Number of Kafka messages received")
        .register(registry)
    
    val kafkaMessagesPublished: Counter = Counter.builder("shipping.kafka.messages.published")
        .description("Number of Kafka messages published")
        .register(registry)
    
    // Timers
    val shippingProcessingTime: Timer = Timer.builder("shipping.processing.time")
        .description("Time taken to process shipping requests")
        .register(registry)
    
    // Gauges
    val activeShipments: AtomicInteger = registry.gauge(
        "shipping.active.shipments",
        AtomicInteger(0)
    ) ?: AtomicInteger(0)
    
    // Helper functions for common increments
    fun incrementOrdersFulfilled() = ordersFulfilled.increment()
    fun incrementOrdersCancelled() = ordersCancelled.increment()
}