package com.scrapybara.kw.shipping.config

import org.springframework.context.annotation.Bean
import org.springframework.context.annotation.Configuration
import org.springframework.kafka.annotation.EnableKafka
import org.springframework.kafka.core.KafkaTemplate
import org.springframework.retry.annotation.EnableRetry
import org.springframework.retry.backoff.ExponentialBackOffPolicy
import org.springframework.retry.policy.SimpleRetryPolicy
import org.springframework.retry.support.RetryTemplate

@Configuration
@EnableRetry
class RetryConfig {
    
    @Bean
    fun retryTemplate(): RetryTemplate {
        val retryTemplate = RetryTemplate()
        
        // Configure exponential backoff
        val backOffPolicy = ExponentialBackOffPolicy()
        backOffPolicy.initialInterval = 1000L    // 1 second
        backOffPolicy.multiplier = 2.0           // double the interval each time
        backOffPolicy.maxInterval = 30000L       // max 30 seconds
        retryTemplate.setBackOffPolicy(backOffPolicy)
        
        // Configure retry policy
        val retryPolicy = SimpleRetryPolicy()
        retryPolicy.maxAttempts = 5              // retry up to 5 times
        retryTemplate.setRetryPolicy(retryPolicy)
        
        return retryTemplate
    }
    
    @Bean
    fun kafkaOperations(kafkaTemplate: KafkaTemplate<String, ByteArray>, retryTemplate: RetryTemplate): KafkaOperations {
        return KafkaOperations(kafkaTemplate, retryTemplate)
    }
}

class KafkaOperations(
    private val kafkaTemplate: KafkaTemplate<String, ByteArray>,
    private val retryTemplate: RetryTemplate
) {
    
    fun sendWithRetry(topic: String, payload: ByteArray) {
        retryTemplate.execute<Void, Exception> {
            kafkaTemplate.send(topic, payload).get() // Wait for the send to complete to catch errors
            null
        }
    }
}