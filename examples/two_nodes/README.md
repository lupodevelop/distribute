# two_nodes

Multi-node integration scripts for the `distribute` library.

All scripts:

- build the library from the repo root (`gleam build`)
- start multiple local BEAM nodes using `erl`
- run a scenario
- print logs and exit

## Prerequisites

- `gleam` in your `PATH`
- Erlang/OTP installed (`erl`, `erlc`)

## Scripts

### `integration_test.sh`

A minimal “two node” end-to-end check:

- starts node A, registers a global name (`calculator`)
- starts node B, connects to node A, sends a message via `distribute/messaging.send_global`
- verifies node A received the message

Run:

```sh
./examples/two_nodes/integration_test.sh
```

### `swim_integration.sh`

Starts 3 nodes and runs the SWIM-like membership service, then queries membership/metrics via RPC.

Run:

```sh
./examples/two_nodes/swim_integration.sh
```

### `partition_rejoin.sh`

Starts 3 nodes, simulates a “partition” by stopping the membership service on node B, then starts it again and prints membership views.

Run:

```sh
./examples/two_nodes/partition_rejoin.sh
```

### `raft_failover_test.sh`

Starts 3 nodes, triggers Raft-lite election, kills the leader node, and checks that a new leader is elected.

Run:

```sh
./examples/two_nodes/raft_failover_test.sh
```

## Notes

- These scripts use Erlang shortnames (`-sname`) and derive node names from `hostname -s`.
- They use a fixed cookie inside each script; run scripts one at a time to avoid clashes.
