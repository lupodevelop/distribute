# Examples

This folder contains small, runnable integration examples for the `distribute` library.

> These examples are intended for local development and manual verification. They start real distributed BEAM nodes (`erl`) on your machine.

## Prerequisites

- `gleam` in your `PATH`
- Erlang/OTP installed (so `erl` and `erlc` are available)

## What’s here

- `two_nodes/` — shell scripts that build the library and run multi-node scenarios (messaging, SWIM membership, partitions, leader failover)
- `two_nodes_app/` — a tiny example Gleam project that wraps the main integration script

## Running

From the repository root:

```sh
./examples/two_nodes/integration_test.sh
```

Or from `examples/two_nodes_app/`:

```sh
./scripts/run_integration.sh
```
