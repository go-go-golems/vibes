package com.scrapybara.kw.shipping.orchestration

import com.fasterxml.jackson.databind.ObjectMapper
import com.scrapybara.kw.shipping.messages.* 
import kotlinx.coroutines.future.await
import kotlinx.coroutines.time.withTimeout
import org.slf4j.LoggerFactory
import org.springframework.kafka.annotation.KafkaListener
import org.springframework.kafka.core.KafkaTemplate
import org.springframework.stereotype.Service
import java.time.Duration
import java.util.UUID
import java.util.concurrent.CompletableFuture
import java.util.concurrent.ConcurrentHashMap

@Service
class ShippingOrchestrator(
    // Using a specific template for internal JSON messages
    private val internalKafkaTemplate: KafkaTemplate<String, String>,
    private val objectMapper: ObjectMapper
) {
    private val logger = LoggerFactory.getLogger(ShippingOrchestrator::class.java)
    private val pendingFutures = ConcurrentHashMap<String, CompletableFuture<ShipmentResult>>() 

    // Define internal topics
    companion object {
        const val COMMAND_TOPIC = "shipping.commands"
        const val EVENT_TOPIC = "shipping.events"
        private val TIMEOUT = Duration.ofSeconds(60) // Configurable timeout
    }

    /**
     * Initiates the shipping process asynchronously via Kafka events
     * but waits for the final result synchronously using suspend.
     */
    suspend fun initiateShipping(orderId: String, allItemsAvailable: Boolean): ShipmentResult {
        val correlationId = UUID.randomUUID().toString()
        val future = CompletableFuture<ShipmentResult>()
        pendingFutures[correlationId] = future
        logger.info("Initiating shipping for order: $orderId with correlationId: $correlationId")

        val startCommand = StartShippingCommand(
            correlationId = correlationId,
            orderId = orderId,
            allItemsAvailable = allItemsAvailable
        )

        try {
            val commandJson = objectMapper.writeValueAsString(startCommand)
            internalKafkaTemplate.send(COMMAND_TOPIC, correlationId, commandJson)
            logger.info("Published StartShippingCommand for correlationId: $correlationId")

            // Suspend waiting for the result or timeout
            return withTimeout(TIMEOUT) {
                logger.debug("Waiting for result for correlationId: $correlationId")
                val result = future.await() // Suspend until future is completed by the listener
                logger.info("Received result for correlationId: $correlationId")
                result
            }
        } catch (e: kotlinx.coroutines.TimeoutCancellationException) {
            logger.error("Shipping process timed out for correlationId: $correlationId after $TIMEOUT")
            pendingFutures.remove(correlationId) // Clean up
            throw ShippingTimeoutException("Shipping process for order $orderId timed out")
        } catch (e: Exception) {
            logger.error("Error during shipping initiation for correlationId: $correlationId", e)
             pendingFutures.remove(correlationId) // Clean up
             throw e // Rethrow other exceptions
        } finally {
             // Ensure cleanup happens even if await() throws cancellation internally other than timeout
             pendingFutures.remove(correlationId)
        }
    }

    /**
     * Listens to the internal event topic for final results.
     */
    @KafkaListener(topics = [EVENT_TOPIC], groupId = "shipping-orchestrator") // Unique group id
    fun handleShippingResult(message: String) {
        logger.debug("Received message on internal event topic: {}", message)
        try {
            val resultEvent = objectMapper.readValue(message, ShippingResultEvent::class.java)
            val correlationId = resultEvent.correlationId
            
            logger.info("Processing result event for correlationId: $correlationId, Status: ${resultEvent.status}")

            val future = pendingFutures[correlationId]
            if (future == null) {
                logger.warn("Received result for unknown or timed-out correlationId: $correlationId")
                return // Ignore if no longer waiting
            }

            when (resultEvent.status) {
                ShippingResultStatus.COMPLETED -> {
                    if (resultEvent.shippingId != null && resultEvent.trackingNumber != null) {
                         val finalResult = ShipmentResult(
                             orderId = resultEvent.orderId,
                             shippingId = resultEvent.shippingId,
                             trackingNumber = resultEvent.trackingNumber
                         )
                         future.complete(finalResult)
                         logger.info("Completed future successfully for correlationId: $correlationId")
                    } else {
                        logger.error("Received COMPLETED status but missing shippingId/trackingNumber for correlationId: $correlationId")
                        future.completeExceptionally(ShippingFailedException("Internal error: Completed status with missing data"))
                    }
                   
                }
                ShippingResultStatus.FAILED -> {
                     val reason = resultEvent.failureReason ?: "Unknown failure"
                     logger.warn("Completing future exceptionally for correlationId: $correlationId. Reason: $reason")
                     future.completeExceptionally(ShippingFailedException(reason))
                }
            }
            // Don't remove from map here, finally block in initiateShipping handles it

        } catch (e: Exception) {
            logger.error("Failed to process internal shipping result event: $message", e)
            // Potentially notify an error monitoring system
        }
    }
} 