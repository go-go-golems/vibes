package com.scrapybara.kw.shipping.models

import java.time.Instant

data class ShipmentStatus(
    val orderId: String,
    val shippingId: String,
    val trackingNumber: String,
    val status: String,
    val createdAt: Instant,
    val updatedAt: Instant,
    val estimatedDelivery: Instant? = null,
    val currentLocation: String? = null,
    val events: List<ShipmentEvent> = emptyList()
)

data class ShipmentEvent(
    val timestamp: Instant,
    val status: String,
    val location: String?,
    val description: String
)