package com.scrapybara.kw.shipping.messages

// Command to initiate the shipping process
data class StartShippingCommand(
    val correlationId: String,
    val orderId: String,
    val allItemsAvailable: Boolean
)

// Enum for result status
enum class ShippingResultStatus { COMPLETED, FAILED }

// Event published internally upon completion or failure
data class ShippingResultEvent(
    val correlationId: String,
    val status: ShippingResultStatus,
    val orderId: String,
    val shippingId: String? = null,     // Included on success
    val trackingNumber: String? = null, // Included on success
    val failureReason: String? = null   // Included on failure
)

// Simple wrapper for the final result returned to the suspended caller
data class ShipmentResult(
    val orderId: String,
    val shippingId: String,
    val trackingNumber: String
)

// Custom exception for handled failures
class ShippingFailedException(message: String) : RuntimeException(message)

// Custom exception for timeouts
class ShippingTimeoutException(message: String) : RuntimeException(message) 