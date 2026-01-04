package com.scrapybara.kw.shipping.services

import com.fasterxml.jackson.databind.ObjectMapper
import com.scrapybara.kw.idl.OrderProto.OrderCancelled
import com.scrapybara.kw.idl.OrderProto.OrderFulfilled
import com.scrapybara.kw.shipping.messages.ShippingResultEvent
import com.scrapybara.kw.shipping.messages.ShippingResultStatus
import com.scrapybara.kw.shipping.messages.StartShippingCommand
import com.scrapybara.kw.shipping.orchestration.ShippingOrchestrator
import org.slf4j.LoggerFactory
import org.springframework.kafka.annotation.KafkaListener
import org.springframework.kafka.core.KafkaTemplate
import org.springframework.stereotype.Service
import java.time.Instant
import java.util.*

// Removed OrderReadyForShippingEvent as it's replaced by StartShippingCommand

@Service
class ShippingService(
    // Kafka template for *external* protobuf messages
    private val externalKafkaTemplate: KafkaTemplate<String, ByteArray>,
    // Kafka template for *internal* JSON messages
    private val internalKafkaTemplate: KafkaTemplate<String, String>,
    private val shippingTrackerService: ShippingTrackerService,
    private val objectMapper: ObjectMapper
) {
    private val logger = LoggerFactory.getLogger(ShippingService::class.java)

    // Listen to the internal command topic defined in the orchestrator
    @KafkaListener(topics = [ShippingOrchestrator.COMMAND_TOPIC], groupId = "shipping-service-processor")
    fun handleShippingCommand(message: String) {
        logger.info("Received message on command topic: {}", message)
        lateinit var command: StartShippingCommand
        try {
            command = objectMapper.readValue(message, StartShippingCommand::class.java)
            logger.info("Processing StartShippingCommand for order: ${command.orderId}, correlationId: ${command.correlationId}")
            processShippingRequest(command)
        } catch (e: Exception) {
            logger.error("Failed to process command message: $message", e)
            // If we can extract correlationId, publish a failure event
            // Otherwise, this is a poison pill or unparseable message
            // For now, just logging. Consider DLQ.
        }
    }

    // Renamed parameter, now takes the command object
    private fun processShippingRequest(command: StartShippingCommand) { 
        val orderId = command.orderId
        val correlationId = command.correlationId
        // Removed logger line here, handled in the listener

        var shippingId: String? = null
        var trackingNumber: String? = null

        try {
            // Step 1: Check Inventory (Simulated)
            logger.info("[${correlationId}] Checking inventory for order: $orderId")
            if (!command.allItemsAvailable) {
                throw ShippingProcessException("Inventory not available for order $orderId")
            }
            logger.info("[${correlationId}] Inventory available for order: $orderId")

            // Step 2: Create Shipping Label (Simulated Generation)
            logger.info("[${correlationId}] Creating shipping label for order: $orderId")
            shippingId = UUID.randomUUID().toString()
            trackingNumber = "TRACK-${UUID.randomUUID().toString().substring(0, 8)}"
            // val shippingLabel = "LABEL-$trackingNumber" // Not used further, but kept for parity
            logger.info("[${correlationId}] Generated shippingId: $shippingId, trackingNumber: $trackingNumber for order: $orderId")

            // Step 3: Confirm with Carrier (Simulated)
            logger.info("[${correlationId}] Confirming shipment with carrier for order: $orderId")
            if (orderId.hashCode() % 10 == 0) { // Same failure condition as saga
                 throw ShippingProcessException("Carrier API failure simulation for order $orderId")
            }
            logger.info("[${correlationId}] Carrier confirmation successful for order: $orderId")

            // Step 4: Create Shipment in Tracker
            logger.info("[${correlationId}] Creating shipment in tracker for order: $orderId")
            shippingTrackerService.createShipment(
                orderId,
                shippingId, // Now non-null due to successful steps
                trackingNumber // Now non-null
            )
            logger.info("[${correlationId}] Shipment created in tracker for order: $orderId")

            // Step 5: Send *External* Notification (Publish OrderFulfilled)
            logger.info("[${correlationId}] Publishing *external* OrderFulfilled event for order: $orderId")
            publishOrderFulfilled(orderId, shippingId, trackingNumber)
            logger.info("[${correlationId}] External OrderFulfilled event published for order: $orderId")

            // Publish *Internal* Success Event
            publishInternalResult(correlationId, orderId, shippingId, trackingNumber, ShippingResultStatus.COMPLETED, null)
            logger.info("[${correlationId}] Internal COMPLETED event published.")

            // Optional: Start simulation (if desired)
            // Consider if this should be triggered by an event too
            shippingTrackerService.simulateShipmentProgress(orderId)

        } catch (e: ShippingProcessException) {
            logger.error("[${correlationId}] Shipping process failed for order $orderId: ${e.message}")
            // Publish *External* OrderCancelled event
            publishOrderCancelled(orderId, "Shipping failed: ${e.message}")
            // Publish *Internal* Failure Event
            publishInternalResult(correlationId, orderId, null, null, ShippingResultStatus.FAILED, e.message)
            logger.warn("[${correlationId}] Internal FAILED event published.")

        } catch (e: Exception) {
            // Catch unexpected errors
            logger.error("[${correlationId}] Unexpected error during shipping process for order $orderId", e)
            // Publish *External* OrderCancelled event (optional, depending on desired behavior for unexpected errors)
            publishOrderCancelled(orderId, "Shipping failed due to unexpected error: ${e.message}")
            // Publish *Internal* Failure Event
            publishInternalResult(correlationId, orderId, null, null, ShippingResultStatus.FAILED, "Unexpected error: ${e.message}")
            logger.error("[${correlationId}] Internal FAILED event published due to unexpected error.")
        }
    }

    // Extracted external event publishing methods for clarity
    private fun publishOrderFulfilled(orderId: String, shippingId: String, trackingNumber: String) {
         val orderFulfilled = OrderFulfilled.newBuilder()
             .setOrderId(orderId)
             .setShippingId(shippingId)
             .setTrackingNumber(trackingNumber)
             .setStatus("shipped")
             .setTimestamp(Instant.now().toEpochMilli().toString())
             .build()
         externalKafkaTemplate.send("order.fulfilled", orderFulfilled.toByteArray())
    }

    private fun publishOrderCancelled(orderId: String, reason: String) {
         logger.warn("Publishing *external* OrderCancelled event for order $orderId. Reason: $reason")
         val orderCancelled = OrderCancelled.newBuilder()
             .setOrderId(orderId)
             .setReason(reason)
             .setRefundStatus("pending")
             .setTimestamp(Instant.now().toEpochMilli().toString())
             .build()
         externalKafkaTemplate.send("order.cancelled", orderCancelled.toByteArray())
    }

    // Method to publish the internal result event
    private fun publishInternalResult(
        correlationId: String, 
        orderId: String, 
        shippingId: String?, 
        trackingNumber: String?,
        status: ShippingResultStatus, 
        failureReason: String? 
    ) {
        val resultEvent = ShippingResultEvent(
            correlationId = correlationId,
            status = status,
            orderId = orderId,
            shippingId = shippingId,
            trackingNumber = trackingNumber,
            failureReason = failureReason
        )
        try {
            val eventJson = objectMapper.writeValueAsString(resultEvent)
            internalKafkaTemplate.send(ShippingOrchestrator.EVENT_TOPIC, correlationId, eventJson)
        } catch (e: Exception) {
             logger.error("[${correlationId}] Failed to serialize or publish internal result event: $resultEvent", e)
             // This is problematic, the orchestrator won't know the result.
             // Consider alerting/monitoring.
        }
    }
}

// Custom exception for handled failures within the shipping process
class ShippingProcessException(message: String) : RuntimeException(message) 