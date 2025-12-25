# two_nodes_app

Tiny example project showing how to run a local two-node integration scenario using the `distribute` library from this repository.

This project does **not** depend on a published Hex version of `distribute`. Instead, the integration script builds the library at the repository root and then starts Erlang/BEAM nodes using the produced beams (`build/dev/erlang/distribute/ebin`).

## Files

- `examples_node_a.gleam` — example code for node A
- `examples_node_b.gleam` — example code for node B
- `scripts/run_integration.sh` — wrapper that runs the main integration script

## Prerequisites

- `gleam` in your `PATH`
- Erlang/OTP installed (`erl`, `erlc`)

## Run

From this directory:

```sh
./scripts/run_integration.sh
```

This invokes `examples/two_nodes/integration_test.sh` from the repository root.
