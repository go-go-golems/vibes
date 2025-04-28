package com.scrapybara.kw.shipping.controllers

import com.scrapybara.kw.shipping.models.ShipmentStatus
import com.scrapybara.kw.shipping.services.ShippingTrackerService
import org.springframework.http.ResponseEntity
import org.springframework.web.bind.annotation.*
import java.util.*

@RestController
@RequestMapping("/api/shipments")
class ShippingController(private val shippingTrackerService: ShippingTrackerService) {

    @GetMapping("/{orderId}")
    fun getShipmentByOrderId(@PathVariable orderId: String): ResponseEntity<ShipmentStatus> {
        val shipment = shippingTrackerService.getShipmentByOrderId(orderId)
        return if (shipment != null) {
            ResponseEntity.ok(shipment)
        } else {
            ResponseEntity.notFound().build()
        }
    }

    @GetMapping("/tracking/{trackingNumber}")
    fun getShipmentByTrackingNumber(@PathVariable trackingNumber: String): ResponseEntity<ShipmentStatus> {
        val shipment = shippingTrackerService.getShipmentByTrackingNumber(trackingNumber)
        return if (shipment != null) {
            ResponseEntity.ok(shipment)
        } else {
            ResponseEntity.notFound().build()
        }
    }

    @GetMapping
    fun getAllShipments(): ResponseEntity<List<ShipmentStatus>> {
        return ResponseEntity.ok(shippingTrackerService.getAllShipments())
    }
}