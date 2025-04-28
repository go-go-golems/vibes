package com.scrapybara.kw.shipping.controllers

import com.scrapybara.kw.shipping.models.ShipmentStatus
import com.scrapybara.kw.shipping.services.ShippingTrackerService
import org.junit.jupiter.api.Test
import org.junit.jupiter.api.extension.ExtendWith
import org.mockito.Mockito.`when`
import org.mockito.kotlin.any
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest
import org.springframework.boot.test.mock.mockito.MockBean
import org.springframework.http.MediaType
import org.springframework.test.context.junit.jupiter.SpringExtension
import org.springframework.test.web.servlet.MockMvc
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get
import org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath
import org.springframework.test.web.servlet.result.MockMvcResultMatchers.status
import java.time.Instant

@ExtendWith(SpringExtension::class)
@WebMvcTest(ShippingController::class)
class ShippingControllerTest {

    @Autowired
    private lateinit var mockMvc: MockMvc

    @MockBean
    private lateinit var shippingTrackerService: ShippingTrackerService

    @Test
    fun `should return shipment when found by order ID`() {
        // Given
        val orderId = "order-123"
        val mockShipment = ShipmentStatus(
            orderId = orderId,
            shippingId = "shipping-456",
            trackingNumber = "TRACK-789",
            status = "IN_TRANSIT",
            createdAt = Instant.now(),
            updatedAt = Instant.now(),
            currentLocation = "Distribution Center"
        )
        
        `when`(shippingTrackerService.getShipmentByOrderId(orderId)).thenReturn(mockShipment)
        
        // When & Then
        mockMvc.perform(get("/api/shipments/{orderId}", orderId)
            .contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk)
            .andExpect(jsonPath("$.orderId").value(orderId))
            .andExpect(jsonPath("$.status").value("IN_TRANSIT"))
    }
    
    @Test
    fun `should return 404 when shipment not found by order ID`() {
        // Given
        val orderId = "non-existent-order"
        `when`(shippingTrackerService.getShipmentByOrderId(orderId)).thenReturn(null)
        
        // When & Then
        mockMvc.perform(get("/api/shipments/{orderId}", orderId)
            .contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isNotFound)
    }
    
    @Test
    fun `should return shipment when found by tracking number`() {
        // Given
        val trackingNumber = "TRACK-789"
        val mockShipment = ShipmentStatus(
            orderId = "order-123",
            shippingId = "shipping-456",
            trackingNumber = trackingNumber,
            status = "IN_TRANSIT",
            createdAt = Instant.now(),
            updatedAt = Instant.now(),
            currentLocation = "Distribution Center"
        )
        
        `when`(shippingTrackerService.getShipmentByTrackingNumber(trackingNumber)).thenReturn(mockShipment)
        
        // When & Then
        mockMvc.perform(get("/api/shipments/tracking/{trackingNumber}", trackingNumber)
            .contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk)
            .andExpect(jsonPath("$.trackingNumber").value(trackingNumber))
    }
    
    @Test
    fun `should return all shipments`() {
        // Given
        val mockShipments = listOf(
            ShipmentStatus(
                orderId = "order-1",
                shippingId = "shipping-1",
                trackingNumber = "TRACK-1",
                status = "DELIVERED",
                createdAt = Instant.now(),
                updatedAt = Instant.now()
            ),
            ShipmentStatus(
                orderId = "order-2",
                shippingId = "shipping-2",
                trackingNumber = "TRACK-2",
                status = "IN_TRANSIT",
                createdAt = Instant.now(),
                updatedAt = Instant.now()
            )
        )
        
        `when`(shippingTrackerService.getAllShipments()).thenReturn(mockShipments)
        
        // When & Then
        mockMvc.perform(get("/api/shipments")
            .contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk)
            .andExpect(jsonPath("$[0].orderId").value("order-1"))
            .andExpect(jsonPath("$[1].orderId").value("order-2"))
    }
}