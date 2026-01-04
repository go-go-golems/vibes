package com.scrapybara.kw.shipping.services

import com.scrapybara.kw.shipping.models.ShipmentEvent
import com.scrapybara.kw.shipping.models.ShipmentStatus
import org.slf4j.LoggerFactory
import org.springframework.stereotype.Service
import java.time.Instant
import java.time.temporal.ChronoUnit
import java.util.concurrent.ConcurrentHashMap

@Service
class ShippingTrackerService {
    private val logger = LoggerFactory.getLogger(ShippingTrackerService::class.java)
    
    // In-memory database for shipments (in a real app, this would be a proper database)
    private val shipments = ConcurrentHashMap<String, ShipmentStatus>()
    private val trackingNumberIndex = ConcurrentHashMap<String, String>() // tracking number -> orderId
    
    fun createShipment(orderId: String, shippingId: String, trackingNumber: String): ShipmentStatus {
        logger.info("Creating shipment for order: $orderId with tracking number: $trackingNumber")
        
        val now = Instant.now()
        val estimatedDelivery = now.plus(3, ChronoUnit.DAYS)
        
        val shipment = ShipmentStatus(
            orderId = orderId,
            shippingId = shippingId,
            trackingNumber = trackingNumber,
            status = "PROCESSING",
            createdAt = now,
            updatedAt = now,
            estimatedDelivery = estimatedDelivery,
            currentLocation = "Warehouse",
            events = listOf(
                ShipmentEvent(
                    timestamp = now,
                    status = "PROCESSING",
                    location = "Warehouse",
                    description = "Shipment created and processing started"
                )
            )
        )
        
        shipments[orderId] = shipment
        trackingNumberIndex[trackingNumber] = orderId
        
        return shipment
    }
    
    fun updateShipmentStatus(
        orderId: String, 
        status: String, 
        location: String? = null, 
        description: String? = null
    ): ShipmentStatus? {
        val shipment = shipments[orderId] ?: return null
        
        val now = Instant.now()
        val newEvent = ShipmentEvent(
            timestamp = now,
            status = status,
            location = location,
            description = description ?: "Status updated to $status"
        )
        
        val updatedShipment = shipment.copy(
            status = status,
            currentLocation = location ?: shipment.currentLocation,
            updatedAt = now,
            events = shipment.events + newEvent
        )
        
        shipments[orderId] = updatedShipment
        return updatedShipment
    }
    
    fun getShipmentByOrderId(orderId: String): ShipmentStatus? {
        return shipments[orderId]
    }
    
    fun getShipmentByTrackingNumber(trackingNumber: String): ShipmentStatus? {
        val orderId = trackingNumberIndex[trackingNumber] ?: return null
        return shipments[orderId]
    }
    
    fun getAllShipments(): List<ShipmentStatus> {
        return shipments.values.toList()
    }
    
    fun simulateShipmentProgress(orderId: String) {
        Thread {
            try {
                // Simulate package moving through shipping stages
                Thread.sleep(5000)
                updateShipmentStatus(
                    orderId = orderId,
                    status = "SHIPPED",
                    location = "Distribution Center",
                    description = "Package has left the warehouse"
                )
                
                Thread.sleep(5000)
                updateShipmentStatus(
                    orderId = orderId,
                    status = "IN_TRANSIT",
                    location = "Regional Hub",
                    description = "Package in transit to destination"
                )
                
                Thread.sleep(5000)
                updateShipmentStatus(
                    orderId = orderId,
                    status = "OUT_FOR_DELIVERY",
                    location = "Local Delivery Center",
                    description = "Package is out for delivery"
                )
                
                Thread.sleep(5000)
                updateShipmentStatus(
                    orderId = orderId,
                    status = "DELIVERED",
                    location = "Destination",
                    description = "Package has been delivered"
                )
            } catch (e: Exception) {
                logger.error("Error simulating shipment progress for order $orderId", e)
            }
        }.start()
    }
}