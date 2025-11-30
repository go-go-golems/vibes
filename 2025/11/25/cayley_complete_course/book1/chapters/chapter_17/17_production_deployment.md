
# Chapter 17: Production Deployment

Taking an application from your local development machine to a production environment involves a new set of challenges and considerations. This final chapter will guide you through the best practices for deploying your Cayley-powered applications, from configuration management and running the HTTP server to monitoring, logging, and disaster recovery.

## Configuration Management

A production application should be configurable without requiring code changes. Cayley itself is configured using a `cayley.yml` file. This file allows you to specify the database path, the storage backend, replication options, and other parameters.

```yaml
database:
  backend: bolt
  path: /var/lib/cayley/mygraph.db

http:
  host: 0.0.0.0
  port: 64210
```

It is good practice to manage your application-specific configuration in a similar way, using environment variables or a configuration file. This allows you to easily change settings between your development, staging, and production environments.

## Running the HTTP API Server

Cayley includes a built-in HTTP server that exposes a RESTful API for interacting with your graph. This is the easiest way to get a production-ready endpoint for your application.

You can start the server using the `cayley http` command:

```bash
cayley http --config=/path/to/cayley.yml
```

This will start the server, which by default listens on port 64210. The HTTP API provides endpoints for:

*   Querying the graph using Gizmo and MQL.
*   Writing and deleting quads.
*   Getting information about the database.

For production use, you will typically want to run the Cayley server behind a reverse proxy like Nginx or Caddy. This allows you to handle SSL termination, load balancing, and other cross-cutting concerns in a dedicated layer.

## Monitoring and Logging

In a production environment, it is crucial to have visibility into the health and performance of your application. Cayley exposes a variety of metrics in Prometheus format, which can be scraped by a Prometheus server and visualized in a dashboarding tool like Grafana.

These metrics include:

*   Query latency.
*   Number of quads and nodes.
*   Cache hit rates.
*   Goroutine and memory usage.

Cayley also produces structured logs that can be collected and analyzed using a log management system like the ELK stack (Elasticsearch, Logstash, Kibana) or Loki.

## Backup and Recovery

No production deployment is complete without a solid backup and recovery plan. The best way to back up your Cayley database depends on the storage backend you are using.

*   **BoltDB**: For BoltDB, you can simply take a copy of the database file. This can be done using standard file system tools like `cp` or `rsync`. It is important to ensure that the database is not being written to while you are taking the backup.

*   **PostgreSQL/MongoDB**: For external databases like PostgreSQL or MongoDB, you should use their native backup tools (e.g., `pg_dump` or `mongodump`).

It is essential to regularly test your backup and recovery process to ensure that you can restore your database in the event of a failure.

Deploying a Cayley application to production requires careful attention to configuration, security, monitoring, and reliability. By following these best practices, you can build a robust and scalable knowledge base that is ready for the demands of a real-world application. The exercises for this chapter will guide you through the process of creating a production-ready configuration for a Cayley server and setting up a basic monitoring and logging pipeline.

---

### References

[1] Cayley Documentation. "Configuration." https://cayley.gitbook.io/cayley/getting-started/configuration

[2] Prometheus. "An open-source monitoring system with a dimensional data model." https://prometheus.io/
