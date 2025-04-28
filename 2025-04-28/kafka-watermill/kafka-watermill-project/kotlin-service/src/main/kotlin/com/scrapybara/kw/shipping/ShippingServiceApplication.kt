package com.scrapybara.kw.shipping

import org.springframework.boot.autoconfigure.SpringBootApplication
import org.springframework.boot.runApplication
import org.springframework.context.annotation.Bean
import org.springframework.kafka.annotation.KafkaListener
import org.springframework.kafka.core.KafkaTemplate
import org.springframework.stereotype.Service
import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.module.kotlin.registerKotlinModule
import com.scrapybara.kw.idl.InventoryCheckedKt
import com.scrapybara.kw.idl.OrderProto
import org.slf4j.LoggerFactory
import org.springframework.kafka.core.DefaultKafkaProducerFactory
import org.springframework.kafka.core.DefaultKafkaConsumerFactory
import org.springframework.kafka.core.ConsumerFactory
import org.springframework.kafka.core.ProducerFactory
import org.springframework.kafka.annotation.EnableKafka
import org.springframework.kafka.config.ConcurrentKafkaListenerContainerFactory
import org.springframework.kafka.support.serializer.JsonSerializer
import org.springframework.kafka.support.serializer.JsonDeserializer
import org.apache.kafka.clients.producer.ProducerConfig
import org.apache.kafka.clients.consumer.ConsumerConfig
import org.apache.kafka.common.serialization.StringSerializer
import org.apache.kafka.common.serialization.StringDeserializer
import org.springframework.context.annotation.Configuration
import org.springframework.retry.annotation.EnableRetry
import java.time.Instant
import java.util.UUID
import com.scrapybara.kw.shipping.services.ShippingTrackerService
import com.scrapybara.kw.shipping.config.KafkaOperations
import com.scrapybara.kw.shipping.config.ShippingMetrics
import com.scrapybara.kw.shipping.saga.ShippingSagaManager
import kotlinx.coroutines.runBlocking
import org.apache.kafka.common.serialization.ByteArrayDeserializer
import org.apache.kafka.common.serialization.ByteArraySerializer
import io.micrometer.core.instrument.Timer
import io.micrometer.core.instrument.MeterRegistry
import org.springframework.beans.factory.annotation.Value

@SpringBootApplication
@EnableKafka
@EnableRetry
class ShippingServiceApplication

fun main(args: Array<String>) {
    runApplication<ShippingServiceApplication>(*args)
}

@Configuration
class KafkaConfig {
    private val objectMapper = ObjectMapper().registerKotlinModule()
    
    @Value("\${spring.kafka.bootstrap-servers}")
    private lateinit var bootstrapServers: String
    
    // Producer config
    @Bean
    fun producerFactory(): ProducerFactory<String, ByteArray> {
        val configProps = mapOf(
            ProducerConfig.BOOTSTRAP_SERVERS_CONFIG to bootstrapServers,
            ProducerConfig.KEY_SERIALIZER_CLASS_CONFIG to StringSerializer::class.java,
            ProducerConfig.VALUE_SERIALIZER_CLASS_CONFIG to ByteArraySerializer::class.java
        )
        return DefaultKafkaProducerFactory(configProps)
    }
    
    @Bean
    fun kafkaTemplate(producerFactory: ProducerFactory<String, ByteArray>): KafkaTemplate<String, ByteArray> {
        return KafkaTemplate(producerFactory)
    }
    
    // Consumer config
    @Bean
    fun consumerFactory(): ConsumerFactory<String, ByteArray> {
        val props = mapOf(
            ConsumerConfig.BOOTSTRAP_SERVERS_CONFIG to bootstrapServers,
            ConsumerConfig.GROUP_ID_CONFIG to "shipping-service",
            ConsumerConfig.KEY_DESERIALIZER_CLASS_CONFIG to StringDeserializer::class.java,
            ConsumerConfig.VALUE_DESERIALIZER_CLASS_CONFIG to ByteArrayDeserializer::class.java,
            ConsumerConfig.AUTO_OFFSET_RESET_CONFIG to "earliest"
        )
        return DefaultKafkaConsumerFactory(props)
    }
    
    @Bean
    fun kafkaListenerContainerFactory(consumerFactory: ConsumerFactory<String, ByteArray>): ConcurrentKafkaListenerContainerFactory<String, ByteArray> {
        val factory = ConcurrentKafkaListenerContainerFactory<String, ByteArray>()
        factory.consumerFactory = consumerFactory
        return factory
    }
}

@Service
class ShippingService(
    private val kafkaOperations: KafkaOperations,
    private val shippingTrackerService: ShippingTrackerService,
    private val metrics: ShippingMetrics,
    private val shippingSagaManager: ShippingSagaManager,
    private val registry: MeterRegistry
) {
    private val logger = LoggerFactory.getLogger(ShippingService::class.java)
    
    @KafkaListener(topics = ["inventory.checked"], containerFactory = "kafkaListenerContainerFactory")
    fun handleInventoryChecked(payloadBytes: ByteArray) {
        metrics.kafkaMessagesReceived.increment()
        metrics.ordersProcessed.increment()
        
        val sample = Timer.start(registry)
        try {
            val event = OrderProto.InventoryChecked.parseFrom(payloadBytes)
            
            logger.info("Received inventory checked event for order: ${event.orderId}, all items available: ${event.allItemsAvailable}")
            
            runBlocking {
                shippingSagaManager.startShippingSaga(
                    orderId = event.orderId,
                    allItemsAvailable = event.allItemsAvailable
                )
            }
            
            if (event.allItemsAvailable) {
                metrics.incrementOrdersFulfilled()
            } else {
                metrics.incrementOrdersCancelled()
            }
        } catch (e: Exception) {
            logger.error("Error processing inventory checked event", e)
            throw e  // Allow Spring Kafka to handle retries
        } finally {
            sample?.stop(metrics.shippingProcessingTime)
        }
    }
}