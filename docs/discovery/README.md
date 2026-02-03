# Discovery Module

Service discovery for distributed Gleam/Erlang applications. The discovery module provides automatic peer detection, health monitoring, and event-driven notifications for cluster membership changes.

## Overview

The Discovery module enables nodes in a distributed system to find and track each other automatically. It provides:

- **Automatic peer detection** - Discovers other nodes in the cluster
- **Health monitoring** - Tracks the health status of discovered peers
- **Event subscriptions** - Get notified when peers join, leave, or change status
- **Snapshot queries** - Query the current state of known peers at any time

## Quick Start

### Starting the Discovery Service

```gleam
import distribute/discovery

pub fn main() {
  // Start discovery as part of a supervision tree
  let spec = discovery.child_spec()
  
  // Or start directly
  case discovery.start_link() {
    Ok(handle) -> {
      io.println("Discovery service started!")
      handle
    }
    Error(err) -> {
      panic as "Failed to start discovery"
    }
  }
}
```

### Subscribing to Peer Events

```gleam
import distribute/discovery.{type EventCallback, type SubscriptionId}
import distribute/discovery/types.{PeerUp, PeerDown, PeerUpdate}

pub fn setup_peer_monitoring() {
  // Define a callback for peer events
  let callback: EventCallback = fn(event) {
    case event {
      PeerUp(peer, metadata) -> io.println("New peer: " <> peer)
      PeerDown(peer, reason) -> io.println("Peer left: " <> peer <> " (" <> reason <> ")")
      PeerUpdate(peer, metadata) -> {
        io.println("Peer " <> peer <> " metadata updated")
      }
    }
  }
  
  // Subscribe to events
  case discovery.subscribe(callback) {
    Ok(subscription_id) -> {
      io.println("Subscribed to peer events")
      subscription_id
    }
    Error(err) -> panic as "Failed to subscribe"
  }
}
```

### Querying Peers

```gleam
import distribute/discovery

pub fn list_all_peers() {
  case discovery.snapshot() {
    Ok(peers) -> {
      list.each(peers, fn(peer) {
        io.println("Peer: " <> peer.id <> " at " <> peer.address)
      })
    }
    Error(_) -> io.println("Failed to get peer list")
  }
}

pub fn find_specific_peer(peer_id: String) {
  case discovery.lookup(peer_id) {
    Ok(peer) -> {
      io.println("Found peer: " <> peer.id)
      Ok(peer)
    }
    Error(_) -> {
      io.println("Peer not found: " <> peer_id)
      Error(Nil)
    }
  }
}
```

## API Reference

### Types

#### DiscoveryError

```gleam
pub type DiscoveryError {
  DiscoveryFailed(reason: String)
  PeerNotFound
  SubscriptionFailed
  ServiceUnavailable
}
```

#### PeerInfo

```gleam
pub type PeerInfo {
  PeerInfo(
    id: String,
    address: String,
    port: Int,
    metadata: Dict(String, String),
  )
}
```

#### HealthStatus

```gleam
pub type HealthStatus {
  Up
  Degraded
  Down
}
```

#### SubscriptionId

Opaque type representing a subscription handle for unsubscribing later.

#### AdapterHandle

Opaque type representing the discovery service handle.

### Functions

#### child_spec

```gleam
pub fn child_spec() -> ChildSpecification
```

Returns a child specification for including discovery in a supervision tree. Use this when you want the discovery service to be supervised and automatically restarted on failures.

#### start_link

```gleam
pub fn start_link() -> Result(AdapterHandle, DiscoveryError)
```

Starts the discovery service. Returns a handle that can be used to interact with the service.

#### subscribe

```gleam
pub fn subscribe(callback: EventCallback) -> Result(SubscriptionId, DiscoveryError)
```

Subscribes to peer discovery events. The callback will be invoked whenever peers join, leave, or change health status. Returns a subscription ID that can be used to unsubscribe later.

#### unsubscribe

```gleam
pub fn unsubscribe(id: SubscriptionId) -> Result(Nil, DiscoveryError)
```

Removes a previously registered event subscription.

#### snapshot

```gleam
pub fn snapshot() -> Result(List(PeerInfo), DiscoveryError)
```

Returns a snapshot of all currently known peers. This is a point-in-time view and may change immediately after the call returns.

#### lookup

```gleam
pub fn lookup(peer_id: String) -> Result(PeerInfo, DiscoveryError)
```

Looks up a specific peer by its ID. Returns the peer information if found, or an error if the peer is not known.

#### health

```gleam
pub fn health() -> HealthStatus
```

Returns the current health status of the discovery service itself.

- `Up` - Service is fully operational
- `Degraded` - Service is operational but experiencing issues
- `Down` - Service is not operational

## Usage Patterns

### Pattern 1: Supervised Discovery Service

Include discovery in your application's supervision tree for automatic restart on failures:

```gleam
import gleam/otp/supervisor
import distribute/discovery

pub fn start_app() {
  supervisor.start(fn(children) {
    children
    |> supervisor.add(discovery.child_spec())
    |> supervisor.add(my_other_service.child_spec())
  })
}
```

### Pattern 2: Reactive Peer Handling

React to peer changes in real-time for dynamic load balancing or failover:

```gleam
import distribute/discovery

pub fn setup_reactive_cluster() {
  let state = new_cluster_state()
  
  let callback = fn(event) {
    case event {
      PeerJoined(peer) -> {
        // Add peer to load balancer
        add_to_pool(state, peer)
      }
      PeerLeft(peer) -> {
        // Remove peer and redistribute work
        remove_from_pool(state, peer)
        redistribute_work(state)
      }
      PeerHealthChanged(peer, Down) -> {
        // Mark peer as unhealthy, avoid routing
        mark_unhealthy(state, peer)
      }
      PeerHealthChanged(peer, Up) -> {
        // Peer recovered, resume routing
        mark_healthy(state, peer)
      }
      _ -> Nil
    }
  }
  
  discovery.subscribe(callback)
}
```

### Pattern 3: Health-Based Routing

Query peer health before making requests:

```gleam
import distribute/discovery

pub fn select_healthy_peer() -> Result(PeerInfo, Nil) {
  case discovery.snapshot() {
    Ok(peers) -> {
      peers
      |> list.filter(fn(peer) { peer.status == Up })
      |> list.first()
    }
    Error(_) -> Error(Nil)
  }
}
```

## Best Practices

1. **Always use supervision** - Start discovery via `child_spec()` in a supervision tree to ensure automatic recovery from failures.

2. **Handle subscription cleanup** - Always call `unsubscribe()` when your process terminates to avoid memory leaks and stale callbacks.

3. **Don't cache snapshots** - The peer list changes frequently. Always call `snapshot()` when you need current data rather than caching old results.

4. **Check service health** - Before relying on discovery data, check `health()` to ensure the service is operational.

5. **Handle errors gracefully** - All operations can fail. Always pattern match on the `Result` type and have fallback behavior.

6. **Use events for real-time needs** - For immediate reactions to cluster changes, use `subscribe()`. For periodic checks, `snapshot()` is sufficient.

## Related Modules

- [Cluster](../cluster/README.md) - Higher-level cluster management
- [Registry](../registry/README.md) - Process registration across nodes
- [Monitor](../monitor/README.md) - Process monitoring primitives
