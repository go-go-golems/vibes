package com.scrapybara.kw.idl

import java.time.Instant
import com.fasterxml.jackson.annotation.JsonProperty
import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.module.kotlin.registerKotlinModule

// Data classes representing order events

data class OrderItem(
    @JsonProperty("product_id") val productId: String,
    @JsonProperty("name") val name: String,
    @JsonProperty("quantity") val quantity: Int,
    @JsonProperty("price") val price: Double
)

data class UnavailableItem(
    @JsonProperty("product_id") val productId: String,
    @JsonProperty("name") val name: String,
    @JsonProperty("requested_quantity") val requestedQuantity: Int,
    @JsonProperty("available_quantity") val availableQuantity: Int
)

data class OrderCreated(
    @JsonProperty("order_id") val orderId: String,
    @JsonProperty("user_id") val userId: String,
    @JsonProperty("items") val items: List<OrderItem>,
    @JsonProperty("total_amount") val totalAmount: Double,
    @JsonProperty("timestamp") val timestamp: Instant
)

data class PaymentProcessed(
    @JsonProperty("order_id") val orderId: String,
    @JsonProperty("payment_id") val paymentId: String,
    @JsonProperty("status") val status: String, // "success", "failed", "pending"
    @JsonProperty("transaction_id") val transactionId: String,
    @JsonProperty("amount") val amount: Double,
    @JsonProperty("timestamp") val timestamp: Instant
)

data class InventoryChecked(
    @JsonProperty("order_id") val orderId: String,
    @JsonProperty("all_items_available") val allItemsAvailable: Boolean,
    @JsonProperty("unavailable_items") val unavailableItems: List<UnavailableItem>,
    @JsonProperty("timestamp") val timestamp: Instant
)

data class OrderFulfilled(
    @JsonProperty("order_id") val orderId: String,
    @JsonProperty("shipping_id") val shippingId: String,
    @JsonProperty("tracking_number") val trackingNumber: String,
    @JsonProperty("status") val status: String,
    @JsonProperty("timestamp") val timestamp: Instant
)

data class OrderCancelled(
    @JsonProperty("order_id") val orderId: String,
    @JsonProperty("reason") val reason: String,
    @JsonProperty("refund_status") val refundStatus: String,
    @JsonProperty("timestamp") val timestamp: Instant
)

data class Notification(
    @JsonProperty("order_id") val orderId: String,
    @JsonProperty("notification_type") val notificationType: String, // "order_confirmation", "payment_success", etc.
    @JsonProperty("recipient") val recipient: String,
    @JsonProperty("content") val content: String,
    @JsonProperty("timestamp") val timestamp: Instant
)

// Serialization helpers
object EventSerializer {
    private val mapper = ObjectMapper().registerKotlinModule()

    fun toJson(event: Any): ByteArray {
        return mapper.writeValueAsBytes(event)
    }

    inline fun <reified T> fromJson(data: ByteArray): T {
        return mapper.readValue(data, T::class.java)
    }
}