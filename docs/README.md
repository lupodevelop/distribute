# `distribute` documentation

Typed distributed messaging for Gleam on the BEAM.

## Read in this order

1. [Quickstart](./quickstart.md). Five minutes from `gleam new` to
   working cross-node messaging.
2. [Recipes](./recipes.md). Copy-pasteable patterns for the common
   shapes: counter, pool, versioned protocol, cluster events,
   observability hooks, custom record codecs.
3. [Getting started](./getting_started.md). Installation, node start,
   configuration in detail.

## Reference

- [Actors and registry](./actors_and_registry.md). `TypedName`,
  `start_registered`, `start_supervised`, `pool`, observed variants,
  global lookup.
- [Messaging](./messaging.md). `send` / `receive` / `call`,
  monitor-based fast-fail, late-reply draining, cluster events.
- [Codecs and types](./codecs_and_types.md). Built-in primitives,
  composites, `codec.map`, tagged messages, wire format.

## Operations

- [Safety and limits](./safety_and_limits.md). Payload size enforcement,
  error hierarchy, atom-table safety, threat model, recommended payload
  limits per workload.

Health snapshots are deterministic: `health().reachable_nodes` and
`health().unreachable_nodes` are returned in `connected_nodes` order.

## API surface at a glance

|Concern|Facade entry points|
|---|---|
|Config|`configure`, `get_config`, `version`, `Config`, `ConfigError`|
|Node lifecycle|`start_node`, `connect`, `nodes`, `self_node`, `is_distributed`, `has_peers`, `health`|
|Cluster events|`start_monitor`, `subscribe`, `unsubscribe`, `ClusterEvent`|
|Actors|`start_actor` / `start_actor_with_timeout` / `start_actor_observed`, `start_registered` / `start_registered_with_timeout`, `start_supervised` / `start_supervised_with_timeout`, `pool` / `pool_with_timeout`, `child_spec` / `child_spec_with_timeout`, `HandlerStep`|
|Registry|`named`, `new_subject`, `from_name`, `register`, `lookup`, `unregister`, `unregister_typed`|
|Messaging|`send`, `reply`, `receive` / `receive_with_timeout`, `call` / `call_with_timeout`, `call_isolated` / `call_isolated_with_timeout`|
|Errors|All public error types (`CallError`, `RegisterError`, ...) and `*_to_string` formatters re-exported on `distribute`|

Low-level modules (`distribute/global`, `distribute/registry`,
`distribute/codec`, `distribute/receiver`, `distribute/cluster`,
`distribute/cluster_monitor`, `distribute/codec/composite`,
`distribute/codec/tagged`, `distribute/config`) remain available for
advanced cases not covered by the facade.
