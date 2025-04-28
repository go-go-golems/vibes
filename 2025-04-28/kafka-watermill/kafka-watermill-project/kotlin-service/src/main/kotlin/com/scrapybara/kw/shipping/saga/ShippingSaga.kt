package com.scrapybara.kw.shipping.saga

import com.scrapybara.kw.idl.OrderCancelled
import com.scrapybara.kw.idl.OrderFulfilled
import com.scrapybara.kw.shipping.models.ShipmentEvent
import com.scrapybara.kw.shipping.services.ShippingTrackerService
import org.slf4j.LoggerFactory
import org.springframework.kafka.core.KafkaTemplate
import org.springframework.stereotype.Component
import java.time.Instant
import java.util.*

/**
 * Data for the shipping saga
 */
data class ShippingSagaData(
    override val sagaId: String = UUID.randomUUID().toString(),
    override val startTime: Instant = Instant.now(),
    override var endTime: Instant? = null,
    override var currentStatus: SagaStatus = SagaStatus.STARTED,
    override var compensating: Boolean = false,
    override var error: String? = null,
    
    // Shipping specific data
    val orderId: String,
    var shippingId: String? = null,
    var trackingNumber: String? = null,
    var allItemsAvailable: Boolean = false,
    var inventoryReserved: Boolean = false,
    var shippingLabel: String? = null,
    var carrierConfirmed: Boolean = false,
    var notificationSent: Boolean = false
) : SagaData

/**
 * Component that creates and manages shipping sagas
 */
@Component
class ShippingSagaManager(
    private val kafkaTemplate: KafkaTemplate<String, Any>,
    private val shippingService: ShippingTrackerService
) {
    private val logger = LoggerFactory.getLogger(ShippingSagaManager::class.java)
    
    // Create the saga steps
    private val shippingSteps = listOf(
        // Step 1: Reserve Inventory
        SagaStep<ShippingSagaData>(
            name = "reserve_inventory",
            handler = { data ->
                logger.info("Reserving inventory for order: ${data.orderId}")
                
                // In a real system, this would call an inventory service
                // For demo, we'll simulate based on the allItemsAvailable flag
                if (!data.allItemsAvailable) {
                    throw RuntimeException("Inventory not available for order ${data.orderId}")
                }
                
                // Mark inventory as reserved
                data.inventoryReserved = true
                data
            },
            compensation = { data ->
                logger.info("Releasing inventory for order: ${data.orderId}")
                
                // In a real system, this would call the inventory service to release the hold
                data.inventoryReserved = false
                data
            }
        ),
        
        // Step 2: Create Shipping Label
        SagaStep<ShippingSagaData>(
            name = "create_shipping_label",
            handler = { data ->
                logger.info("Creating shipping label for order: ${data.orderId}")
                
                // Generate shipping ID and tracking number
                data.shippingId = UUID.randomUUID().toString()
                data.trackingNumber = "TRACK-${UUID.randomUUID().toString().substring(0, 8)}"
                data.shippingLabel = "LABEL-${data.trackingNumber}"
                
                // In a real system, this would call a shipping label service
                data
            },
            compensation = { data ->
                logger.info("Voiding shipping label for order: ${data.orderId}")
                
                // In a real system, this would call the shipping label service to void the label
                data.shippingLabel = null
                data
            }
        ),
        
        // Step 3: Confirm with Carrier
        SagaStep<ShippingSagaData>(
            name = "confirm_with_carrier",
            handler = { data ->
                logger.info("Confirming shipment with carrier for order: ${data.orderId}")
                
                // In a real system, this would call a carrier API
                // Simulate a failure sometimes for demonstration purposes
                if (data.orderId.hashCode() % 10 == 0) {
                    throw RuntimeException("Carrier API failure for order ${data.orderId}")
                }
                
                data.carrierConfirmed = true
                data
            },
            compensation = { data ->
                logger.info("Cancelling carrier pickup for order: ${data.orderId}")
                
                // In a real system, this would call the carrier API to cancel
                data.carrierConfirmed = false
                data
            }
        ),
        
        // Step 4: Create Shipment in Tracker
        SagaStep<ShippingSagaData>(
            name = "create_shipment",
            handler = { data ->
                logger.info("Creating shipment in tracker for order: ${data.orderId}")
                
                // Create a shipment record
                shippingService.createShipment(
                    data.orderId,
                    data.shippingId!!,
                    data.trackingNumber!!
                )
                
                data
            },
            compensation = { data ->
                // No compensation needed as failed shipments will be automatically cleaned up
                data
            }
        ),
        
        // Step 5: Send Notification
        SagaStep<ShippingSagaData>(
            name = "send_notification",
            handler = { data ->
                logger.info("Sending shipment notification for order: ${data.orderId}")
                
                // In a real system, this would send an email or SMS
                data.notificationSent = true
                
                // Publish the shipment event to Kafka
                val orderFulfilled = OrderFulfilled(
                    orderId = data.orderId,
                    shippingId = data.shippingId!!,
                    trackingNumber = data.trackingNumber!!,
                    status = "shipped",
                    timestamp = Instant.now()
                )
                
                kafkaTemplate.send("order.fulfilled", orderFulfilled)
                data
            },
            compensation = { data ->
                logger.info("Sending shipment cancellation notification for order: ${data.orderId}")
                
                // In a real system, this would send a cancellation email
                // Publish order cancelled event
                val orderCancelled = OrderCancelled(
                    orderId = data.orderId,
                    reason = "Shipping failed: ${data.error}",
                    refundStatus = "pending",
                    timestamp = Instant.now()
                )
                
                kafkaTemplate.send("order.cancelled", orderCancelled)
                data
            }
        )
    )
    
    // Create the saga coordinator
    private val coordinator = SagaCoordinatorFactory(kafkaTemplate)
        .createCoordinator("shipping_saga", shippingSteps)
    
    /**
     * Start a new shipping saga for an order
     */
    suspend fun startShippingSaga(orderId: String, allItemsAvailable: Boolean): ShippingSagaData {
        val sagaData = ShippingSagaData(
            orderId = orderId,
            allItemsAvailable = allItemsAvailable
        )
        
        return try {
            coordinator.executeSaga(sagaData)
        } catch (e: Exception) {
            logger.error("Shipping saga failed for order $orderId: ${e.message}")
            sagaData
        }
    }
}