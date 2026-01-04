package com.scrapybara.kw.shipping.services

import org.junit.jupiter.api.Test
import org.junit.jupiter.api.Assertions.*
import java.time.Instant

class ShippingTrackerServiceTest {

    private val shippingTrackerService = ShippingTrackerService()

    @Test
    fun `should create a shipment with correct initial state`() {
        // Given
        val orderId = "order-123"
        val shippingId = "shipping-456"
        val trackingNumber = "TRACK-789"

        // When
        val shipment = shippingTrackerService.createShipment(orderId, shippingId, trackingNumber)

        // Then
        assertEquals(orderId, shipment.orderId)
        assertEquals(shippingId, shipment.shippingId)
        assertEquals(trackingNumber, shipment.trackingNumber)
        assertEquals("PROCESSING", shipment.status)
        assertEquals("Warehouse", shipment.currentLocation)
        assertEquals(1, shipment.events.size)
        
        // Verify the shipment can be retrieved
        val retrievedByOrderId = shippingTrackerService.getShipmentByOrderId(orderId)
        assertEquals(shipment, retrievedByOrderId)
        
        val retrievedByTracking = shippingTrackerService.getShipmentByTrackingNumber(trackingNumber)
        assertEquals(shipment, retrievedByTracking)
    }

    @Test
    fun `should update shipment status correctly`() {
        // Given
        val orderId = "order-123"
        val shippingId = "shipping-456"
        val trackingNumber = "TRACK-789"
        shippingTrackerService.createShipment(orderId, shippingId, trackingNumber)

        // When
        val updatedShipment = shippingTrackerService.updateShipmentStatus(
            orderId = orderId,
            status = "IN_TRANSIT",
            location = "Distribution Center",
            description = "Package has left the warehouse"
        )

        // Then
        assertNotNull(updatedShipment)
        assertEquals("IN_TRANSIT", updatedShipment?.status)
        assertEquals("Distribution Center", updatedShipment?.currentLocation)
        assertEquals(2, updatedShipment?.events?.size)
        assertEquals("IN_TRANSIT", updatedShipment?.events?.last()?.status)
    }

    @Test
    fun `should return null when updating non-existent shipment`() {
        // When
        val result = shippingTrackerService.updateShipmentStatus(
            orderId = "non-existent-order",
            status = "IN_TRANSIT"
        )

        // Then
        assertNull(result)
    }

    @Test
    fun `should retrieve all shipments`() {
        // Given
        shippingTrackerService.createShipment("order-1", "shipping-1", "TRACK-1")
        shippingTrackerService.createShipment("order-2", "shipping-2", "TRACK-2")
        shippingTrackerService.createShipment("order-3", "shipping-3", "TRACK-3")

        // When
        val allShipments = shippingTrackerService.getAllShipments()

        // Then
        assertEquals(3, allShipments.size)
    }
}