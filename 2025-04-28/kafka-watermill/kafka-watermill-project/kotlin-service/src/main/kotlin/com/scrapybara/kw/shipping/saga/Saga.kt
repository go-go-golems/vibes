package com.scrapybara.kw.shipping.saga

import com.scrapybara.kw.idl.OrderProto.SagaEvent
import org.slf4j.LoggerFactory
import org.springframework.kafka.core.KafkaTemplate
import java.time.Instant
import java.util.*
import java.util.concurrent.CompletableFuture

/**
 * Represents a step in a saga
 */
class SagaStep<T>(
    val name: String,
    val handler: suspend (T) -> T,
    val compensation: suspend (T) -> T
)

/**
 * Base class for saga data
 */
interface SagaData {
    val sagaId: String
    val startTime: Instant
    var endTime: Instant?
    var currentStatus: SagaStatus
    var compensating: Boolean
    var error: String?
}

enum class SagaStatus {
    STARTED, COMPLETED, FAILED, COMPENSATING, COMPENSATION_COMPLETED
}

/**
 * Main saga coordinator
 */
class SagaCoordinator<T : SagaData>(
    private val sagaName: String,
    private val steps: List<SagaStep<T>>,
    private val kafkaTemplate: KafkaTemplate<String, ByteArray>
) {
    private val logger = LoggerFactory.getLogger(SagaCoordinator::class.java)

    suspend fun executeSaga(data: T): T {
        try {
            logger.info("Starting saga $sagaName with ID ${data.sagaId}")
            publishSagaEvent(data, "STARTED")
            
            // Execute all steps in order
            var currentData = data
            steps.forEachIndexed { index, step ->
                try {
                    logger.info("Executing step ${step.name} for saga ${data.sagaId}")
                    currentData = step.handler(currentData)
                    publishStepEvent(currentData, step.name, "COMPLETED")
                } catch (e: Exception) {
                    logger.error("Error executing step ${step.name}: ${e.message}")
                    publishStepEvent(currentData, step.name, "FAILED", e.message)
                    
                    // Start compensation from the previous step
                    currentData.compensating = true
                    currentData.error = e.message
                    currentData.currentStatus = SagaStatus.COMPENSATING
                    publishSagaEvent(currentData, "COMPENSATING")
                    
                    // Execute compensation steps in reverse order
                    for (i in index - 1 downTo 0) {
                        val compensationStep = steps[i]
                        try {
                            logger.info("Executing compensation for step ${compensationStep.name}")
                            currentData = compensationStep.compensation(currentData)
                            publishStepEvent(currentData, compensationStep.name, "COMPENSATED")
                        } catch (ce: Exception) {
                            logger.error("Error during compensation for step ${compensationStep.name}: ${ce.message}")
                            publishStepEvent(currentData, compensationStep.name, "COMPENSATION_FAILED", ce.message)
                        }
                    }
                    
                    currentData.currentStatus = SagaStatus.COMPENSATION_COMPLETED
                    currentData.endTime = Instant.now()
                    publishSagaEvent(currentData, "COMPENSATION_COMPLETED")
                    
                    throw e
                }
            }
            
            // All steps completed successfully
            currentData.currentStatus = SagaStatus.COMPLETED
            currentData.endTime = Instant.now()
            publishSagaEvent(currentData, "COMPLETED")
            
            return currentData
        } catch (e: Exception) {
            logger.error("Saga $sagaName failed: ${e.message}")
            data.currentStatus = SagaStatus.FAILED
            data.endTime = Instant.now()
            publishSagaEvent(data, "FAILED", e.message)
            throw e
        }
    }
    
    private fun publishSagaEvent(data: T, status: String, error: String? = null) {
        val sagaEvent = SagaEvent.newBuilder()
            .setSagaId(data.sagaId)
            .setSagaName(sagaName)
            .setStatus(status)
            .setTimestamp(Instant.now().toEpochMilli())
            .setError(error ?: "")
            .build()
        
        kafkaTemplate.send("saga.events", data.sagaId, sagaEvent.toByteArray())
            .whenComplete { _, ex ->
                if (ex != null) {
                    logger.error("Error publishing saga event", ex)
                }
            }
    }
    
    private fun publishStepEvent(data: T, stepName: String, status: String, error: String? = null) {
        val stepEventProto = SagaEvent.newBuilder()
            .setSagaId(data.sagaId)
            .setSagaName(sagaName)
            .setStatus("$stepName:$status")
            .setTimestamp(Instant.now().toEpochMilli())
            .setError(error ?: "")
            .build()
        
        kafkaTemplate.send("saga.step.events", data.sagaId, stepEventProto.toByteArray())
            .whenComplete { _, ex ->
                if (ex != null) {
                    logger.error("Error publishing step event for step $stepName", ex)
                }
            }
    }
}

/**
 * Factory to create saga coordinators
 */
class SagaCoordinatorFactory(private val kafkaTemplate: KafkaTemplate<String, ByteArray>) {
    fun <T : SagaData> createCoordinator(sagaName: String, steps: List<SagaStep<T>>): SagaCoordinator<T> {
        return SagaCoordinator(sagaName, steps, kafkaTemplate)
    }
}