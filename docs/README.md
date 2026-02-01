# Documentation

This directory contains comprehensive guides and operational documentation for using the distribute library.

## Getting Started

Start with the appropriate guide for your use case:

### Core Messaging (6 modules)

- **[Codec](codec/)** — How to encode/decode messages for transport
  - SizedDecoder pattern, composable encoders, envelope versioning
  
- **[Global](global/)** — Typed subjects and channels
  - Creating subjects, send/receive, type-safe patterns
  
- **[Messaging](messaging/)** — Sending and receiving messages
  - Send patterns (fire-and-forget, RPC, batch), error handling
  
- **[Registry](registry/)** — Global name service and process discovery
  - Type-safe patterns, retry policies, failover handling
  
- **[Actor](actor/)** — Building typed actors with supervision
  - Actor types, patterns, lifecycle, error handling
  
- **[Retry](retry/)** — Exponential backoff and jitter strategies
  - 4 jitter strategies, preset policies, manual loops

### Cluster & Coordination (3 modules)

- **[Cluster](cluster/)** — Multi-node cluster management
  - Join/leave, node discovery, failure detection, topology
  
- **[Discovery](discovery/)** — Service discovery without hardcoding
  - DNS, file-based, custom backends, health checking
  
- **[Groups](groups/)** — Process grouping and broadcast
  - Dynamic groups, broadcast messaging, worker pools

### Operations & Resources (3 modules)

- **[Monitor](monitor/)** — Process monitoring and failure detection
  - Watch processes, restart on crash, supervisory patterns
  
- **[Connection Pool](connection-pool/)** — Pooled resource management
  - Database pools, HTTP clients, backpressure handling
  
- **[Settings](settings/)** — Runtime configuration management
  - Environment-based config, dynamic updates, validation

### Crypto & Security (1 module)

- **[Crypto](crypto/)** — Encrypted communication between nodes
  - Quick start, adapter pattern, ChaCha20-Poly1305 + X25519
  - Error handling, performance, troubleshooting
  
- **[Crypto Security](crypto/security.md)** — Pre-deploy checklist and operational guidance
  - Pre-deployment verification, incident response, compliance

## Design Materials

Internal design materials are maintained under `/dev/`. Consult project maintainers before publishing or extracting content from that directory.

## Architecture Overview

```
distribute
├── Core Messaging
│   ├── Codec (encode/decode)
│   ├── Messaging (send/receive)
│   ├── Global (type-safe channels)
│   ├── Registry (name service)
│   ├── Actor (stateful processes)
│   └── Retry (error recovery)
├── Cluster & Coordination
│   ├── Cluster (multi-node)
│   ├── Discovery (service lookup)
│   └── Groups (broadcast)
├── Operations & Resources
│   ├── Monitor (process supervision)
│   ├── Connection Pool (resource pooling)
│   └── Settings (configuration)
└── Security
    └── Crypto (encryption)
```

## Quick Navigation

| Need | Guide |
|------|-------|
| "How do I send a message?" | [Messaging](messaging/) |
| "How do I store data globally?" | [Registry](registry/) |
| "How do I create an actor?" | [Actor](actor/) |
| "How do I retry a failing operation?" | [Retry](retry/) |
| "How do I enable encryption?" | [Crypto](crypto/) |
| "How do I encode my message?" | [Codec](codec/) |
| "How do I use type-safe channels?" | [Global](global/) |
| "How do I join a cluster?" | [Cluster](cluster/) |
| "How do I discover services?" | [Discovery](discovery/) |
| "How do I broadcast to a group?" | [Groups](groups/) |
| "How do I monitor a process?" | [Monitor](monitor/) |
| "How do I manage a connection pool?" | [Connection Pool](connection-pool/) |
| "How do I configure my app?" | [Settings](settings/) |

## Feature Matrix

| Feature | Module | Type |
|---------|--------|------|
| **Messaging** | Codec, Messaging | Core |
| **Name Service** | Registry | Core |
| **Process Management** | Actor, Monitor | Core |
| **Resilience** | Retry | Core |
| **Clustering** | Cluster, Discovery, Groups | Coordination |
| **Resource Management** | Connection Pool, Settings | Operations |
| **Security** | Crypto | Security |

## Roadmap

- ✅ v2.0: Core messaging documentation (codec, global, messaging, registry, actor, retry)
- ✅ v2.1: Cluster & coordination docs (cluster, discovery, groups)
- ✅ v2.2: Operations documentation (monitor, connection-pool, settings)
- ✅ v2.3: Crypto user guide complete
- ⏳ v3.0: Performance tuning guide
- ⏳ v3.x: Migration guide from Erlang distribution
- ⏳ v3.x: Advanced patterns (hot code reloading, cluster upgrade)