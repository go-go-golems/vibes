package com.scrapybara.kw.shipping

import com.scrapybara.kw.idl.InventoryChecked
import com.scrapybara.kw.idl.UnavailableItem
import org.apache.kafka.clients.producer.ProducerConfig
import org.apache.kafka.common.serialization.StringSerializer
import org.junit.jupiter.api.Test
import org.junit.jupiter.api.extension.ExtendWith
import org.mockito.kotlin.any
import org.mockito.kotlin.eq
import org.mockito.kotlin.verify
import org.mockito.kotlin.timeout
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.boot.test.context.SpringBootTest
import org.springframework.boot.test.context.TestConfiguration
import org.springframework.boot.test.mock.mockito.MockBean
import org.springframework.context.annotation.Bean
import org.springframework.kafka.core.DefaultKafkaProducerFactory
import org.springframework.kafka.core.KafkaTemplate
import org.springframework.kafka.core.ProducerFactory
import org.springframework.kafka.support.serializer.JsonSerializer
import org.springframework.kafka.test.context.EmbeddedKafka
import org.springframework.test.context.junit.jupiter.SpringExtension
import com.scrapybara.kw.shipping.services.ShippingTrackerService
import java.time.Instant
import java.util.concurrent.TimeUnit

@ExtendWith(SpringExtension::class)
@SpringBootTest
@EmbeddedKafka(partitions = 1, topics = ["inventory.checked"])
class KafkaIntegrationTest {

    @TestConfiguration
    class KafkaProducerConfig {
        @Bean
        fun producerFactory(): ProducerFactory<String, Any> {
            val props = HashMap<String, Any>()
            props[ProducerConfig.BOOTSTRAP_SERVERS_CONFIG] = "localhost:9092"
            props[ProducerConfig.KEY_SERIALIZER_CLASS_CONFIG] = StringSerializer::class.java
            props[ProducerConfig.VALUE_SERIALIZER_CLASS_CONFIG] = JsonSerializer::class.java
            return DefaultKafkaProducerFactory(props)
        }

        @Bean
        fun kafkaTemplate(): KafkaTemplate<String, Any> {
            return KafkaTemplate(producerFactory())
        }
    }

    @Autowired
    private lateinit var kafkaTemplate: KafkaTemplate<String, Any>

    @MockBean
    private lateinit var shippingTrackerService: ShippingTrackerService

    @Test
    fun `should process inventory checked event and create shipment when all items available`() {
        // Given
        val inventoryChecked = InventoryChecked(
            orderId = "test-order-123",
            allItemsAvailable = true,
            unavailableItems = emptyList(),
            timestamp = Instant.now()
        )

        // When
        kafkaTemplate.send("inventory.checked", inventoryChecked).get(10, TimeUnit.SECONDS)

        // Then
        verify(shippingTrackerService, timeout(5000)).createShipment(
            eq("test-order-123"),
            any(),
            any()
        )
    }

    @Test
    fun `should not create shipment when items are unavailable`() {
        // Given
        val inventoryChecked = InventoryChecked(
            orderId = "test-order-456",
            allItemsAvailable = false,
            unavailableItems = listOf(
                UnavailableItem(
                    productId = "product-1",
                    name = "Test Product",
                    requestedQuantity = 2,
                    availableQuantity = 1
                )
            ),
            timestamp = Instant.now()
        )

        // When
        kafkaTemplate.send("inventory.checked", inventoryChecked).get(10, TimeUnit.SECONDS)

        // Then - shippingTrackerService.createShipment should not be called
        verify(shippingTrackerService, timeout(5000).times(0))
            .createShipment(eq("test-order-456"), any(), any())
    }
}