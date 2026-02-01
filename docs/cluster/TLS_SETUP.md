# TLS Setup for Erlang Distribution

> **Version:** 2.1.0  
> **Last updated:** 1 Feb 2026

This guide explains how to configure TLS (Transport Layer Security) for Erlang distribution, enabling secure communication between nodes in a distributed `distribute` cluster.

---

## Why TLS for Distribution?

By default, Erlang distribution uses a **shared cookie** for authentication but **no encryption**. This is acceptable for:
- Single-datacenter deployments on trusted networks
- Development and testing

For **cross-datacenter** or **untrusted network** deployments, you need TLS to:
- ✅ Encrypt all inter-node communication
- ✅ Authenticate nodes via certificates (mTLS)
- ✅ Prevent man-in-the-middle attacks
- ✅ Meet compliance requirements (PCI-DSS, HIPAA, etc.)

---

## Quick Start

### 1. Generate Certificates

For production, use proper CA-signed certificates. For testing:

```bash
# Create CA
openssl genrsa -out ca.key 4096
openssl req -x509 -new -nodes -key ca.key -sha256 -days 365 \
    -out ca.crt -subj "/CN=DistributeCA"

# Create node certificate
openssl genrsa -out node.key 2048
openssl req -new -key node.key -out node.csr \
    -subj "/CN=node@hostname"
openssl x509 -req -in node.csr -CA ca.crt -CAkey ca.key \
    -CAcreateserial -out node.crt -days 365 -sha256
```

### 2. Create SSL Distribution Config

Create `ssl_dist.conf`:

```erlang
[
  {server, [
    {cacertfile, "/path/to/ca.crt"},
    {certfile, "/path/to/node.crt"},
    {keyfile, "/path/to/node.key"},
    {verify, verify_peer},
    {fail_if_no_peer_cert, true},
    {secure_renegotiate, true}
  ]},
  {client, [
    {cacertfile, "/path/to/ca.crt"},
    {certfile, "/path/to/node.crt"},
    {keyfile, "/path/to/node.key"},
    {verify, verify_peer},
    {secure_renegotiate, true}
  ]}
].
```

### 3. Start Node with TLS

```bash
# Using vm.args
erl -proto_dist inet_tls \
    -ssl_dist_optfile /path/to/ssl_dist.conf \
    -name node1@hostname \
    -setcookie mysecretcookie
```

Or in `vm.args`:

```
-proto_dist inet_tls
-ssl_dist_optfile /path/to/ssl_dist.conf
-name node1@hostname
-setcookie mysecretcookie
```

### 4. Verify TLS is Active

```erlang
%% In Erlang shell
net_kernel:get_net_ticktime().
%% Should work normally

%% Check TLS info on connection
ssl:connection_information(Socket).
```

---

## Configuration Options

### Server Options

| Option | Description | Recommended |
|--------|-------------|-------------|
| `cacertfile` | CA certificate path | Required |
| `certfile` | Node certificate path | Required |
| `keyfile` | Private key path | Required |
| `verify` | Peer verification | `verify_peer` |
| `fail_if_no_peer_cert` | Require client cert | `true` for mTLS |
| `depth` | Certificate chain depth | `2` |
| `ciphers` | Allowed cipher suites | See below |

### Recommended Cipher Suites

For OTP 24+:

```erlang
{ciphers, [
    "TLS_AES_256_GCM_SHA384",
    "TLS_AES_128_GCM_SHA256",
    "TLS_CHACHA20_POLY1305_SHA256"
]}
```

For OTP 23 and earlier:

```erlang
{ciphers, [
    "ECDHE-ECDSA-AES256-GCM-SHA384",
    "ECDHE-RSA-AES256-GCM-SHA384",
    "ECDHE-ECDSA-CHACHA20-POLY1305",
    "ECDHE-RSA-CHACHA20-POLY1305"
]}
```

### TLS Version

Enforce TLS 1.3 (OTP 23+):

```erlang
{versions, ['tlsv1.3']}
```

Or TLS 1.2 minimum:

```erlang
{versions, ['tlsv1.3', 'tlsv1.2']}
```

---

## Full Example Configuration

### `ssl_dist.conf` (Production)

```erlang
[
  {server, [
    {cacertfile, "/etc/distribute/certs/ca.crt"},
    {certfile, "/etc/distribute/certs/node.crt"},
    {keyfile, "/etc/distribute/certs/node.key"},
    {verify, verify_peer},
    {fail_if_no_peer_cert, true},
    {depth, 2},
    {versions, ['tlsv1.3', 'tlsv1.2']},
    {secure_renegotiate, true},
    {reuse_sessions, true}
  ]},
  {client, [
    {cacertfile, "/etc/distribute/certs/ca.crt"},
    {certfile, "/etc/distribute/certs/node.crt"},
    {keyfile, "/etc/distribute/certs/node.key"},
    {verify, verify_peer},
    {depth, 2},
    {versions, ['tlsv1.3', 'tlsv1.2']},
    {secure_renegotiate, true},
    {reuse_sessions, true}
  ]}
].
```

### `vm.args` (Production)

```
## Node identity
-name myapp@myhost.example.com
-setcookie ${ERLANG_COOKIE}

## TLS Distribution
-proto_dist inet_tls
-ssl_dist_optfile /etc/distribute/ssl_dist.conf

## Kernel options
-kernel inet_dist_listen_min 9100
-kernel inet_dist_listen_max 9155
```

---

## Gleam/distribute Integration

TLS distribution is **transparent** to `distribute` - once configured at the VM level, all `distribute` functions work unchanged:

```gleam
import distribute/cluster

pub fn main() {
  // These work identically over TLS
  let _ = cluster.start_node("node1@host.example.com", cookie)
  let _ = cluster.connect("node2@host.example.com")
  
  // All messaging is now encrypted
  let _ = messaging.send_typed(subject, message, encoder)
}
```

### Health Check with TLS

Use `cluster.health()` to verify TLS connectivity:

```gleam
import distribute/cluster

pub fn check_cluster_health() {
  case cluster.health() {
    Ok(status) -> {
      io.println("Cluster healthy")
      io.println("Self: " <> status.self_node)
      io.println("Connected: " <> int.to_string(list.length(status.connected_nodes)))
    }
    Error(reason) -> {
      io.println("Cluster unhealthy: " <> reason)
    }
  }
}
```

---

## Docker / Kubernetes Deployment

### Docker Compose

```yaml
version: '3.8'
services:
  node1:
    image: myapp:latest
    environment:
      - ERLANG_COOKIE=mysecretcookie
    volumes:
      - ./certs:/etc/distribute/certs:ro
      - ./ssl_dist.conf:/etc/distribute/ssl_dist.conf:ro
    command: >
      erl -proto_dist inet_tls
          -ssl_dist_optfile /etc/distribute/ssl_dist.conf
          -name node1@node1
          -setcookie $ERLANG_COOKIE
```

### Kubernetes

Use Secrets for certificates:

```yaml
apiVersion: v1
kind: Secret
metadata:
  name: distribute-tls
type: Opaque
data:
  ca.crt: <base64-encoded>
  node.crt: <base64-encoded>
  node.key: <base64-encoded>
---
apiVersion: apps/v1
kind: StatefulSet
metadata:
  name: myapp
spec:
  template:
    spec:
      containers:
        - name: myapp
          volumeMounts:
            - name: tls-certs
              mountPath: /etc/distribute/certs
              readOnly: true
      volumes:
        - name: tls-certs
          secret:
            secretName: distribute-tls
```

---

## Troubleshooting

### Connection Refused

```
=ERROR REPORT==== Node 'node2@host' not responding
```

**Check:**
1. Both nodes use same CA certificate
2. Certificates are not expired: `openssl x509 -in node.crt -noout -dates`
3. Firewall allows distribution ports (default 9100-9155)

### Certificate Verification Failed

```
=ERROR REPORT==== TLS: certificate verify failed
```

**Check:**
1. CN or SAN matches node name
2. CA certificate is the same on both nodes
3. Certificate chain is complete

### Cipher Mismatch

```
=ERROR REPORT==== TLS: no suitable cipher found
```

**Check:**
1. Both nodes support same TLS version
2. Cipher suites overlap between nodes
3. OTP version supports requested ciphers

### Debug TLS

Enable TLS debugging:

```erlang
%% In vm.args or erl command
-ssl_dist_opt server_verify_opts [log_level,debug]
```

---

## Security Best Practices

1. **Use mTLS** - Always set `fail_if_no_peer_cert true`
2. **Rotate certificates** - Automate with cert-manager or similar
3. **Separate CA** - Don't reuse application CA for distribution
4. **Firewall** - Limit distribution ports to known nodes
5. **Monitor** - Alert on certificate expiration
6. **Audit** - Log connection attempts

---

## Performance Considerations

| Aspect | Impact | Mitigation |
|--------|--------|------------|
| Handshake latency | +1-5ms per connection | Connection pooling (handled by BEAM) |
| CPU overhead | ~5-10% for encryption | Use AES-NI capable hardware |
| Memory | Slight increase for SSL state | Minimal in practice |

TLS overhead is **negligible** for most workloads. The BEAM reuses connections, so handshake cost is amortized.

---

## Related Documentation

- [Erlang TLS Distribution](https://www.erlang.org/doc/apps/ssl/ssl_distribution.html)
- [OTP SSL Application](https://www.erlang.org/doc/apps/ssl/ssl.html)
- [distribute Cluster Module](./README.md)

---

*For application-level encryption on top of TLS, see the [Crypto Adapter documentation](../crypto/README.md).*
