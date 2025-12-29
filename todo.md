# TODO: Rendere la libreria `distribute` veramente type-safe

Questa lista contiene i passi pianificati per trasformare `distribute` in una libreria che può legittimamente dichiararsi "type-safe" e che si integra correttamente con l'ecosistema Gleam standard (`gleam_otp`, `gleam_erlang`).

## 🎯 Obiettivo principale (Issue di Louis Pilfold)

L'issue originale evidenzia che:
1. Le API di messaging/RPC sono **interamente untyped** (uso di FFI rimuove type checking)
2. **Nessuna integrazione** con gleam_erlang e gleam_otp
3. Gli utenti devono implementare pattern typed da soli (difficile senza controllare i package)

**Obiettivo**: Rendere la libreria la **reference implementation** per distributed computing type-safe in Gleam, con piena integrazione all'ecosistema standard.

---

## ✅ Stato corrente (Fase 1 completata)

**Completato**:
- ✅ Dipendenze: `gleam_erlang` e `gleam_otp` aggiunte a `gleam.toml`
- ✅ Sistema codec robusto: `Encoder(a)` / `Decoder(a)` con gestione errori esplicita
- ✅ Envelope con versioning: tag + version per validazione runtime
- ✅ FFI Erlang aggiornati: `send_binary_global`, `call_binary_with_timeout`, `broadcast_binary`
- ✅ API typed parallele: `send_typed`, `call_typed`, `broadcast_typed`
- ✅ Custom `Subject(a)` wrapper con tag
- ✅ 187 test (codec, envelope, typed subjects)

**Problemi critici rimasti**:
- ❌ **Zero integrazione effettiva con gleam_otp**: dipendenze aggiunte ma non usate
- ❌ **Subject(a) custom incompatibile** con `gleam/otp/process.Subject(msg)`
- ❌ **Nessun lato receiver typed**: utente deve implementare receive + decode manualmente
- ❌ **API untyped ancora esposte** senza deprecation/warning
- ❌ **Nessun esempio end-to-end** typed sender→receiver

---

## 📋 Fase 2: Integrazione completa con gleam_otp (PRIORITÀ MASSIMA)

### 1. Sostituire `distribute/typed_process.Subject` con `gleam/otp/process.Subject`

**Problema**: Abbiamo creato un tipo `Subject(a)` custom incompatibile con l'ecosistema.

**Azione**:
- Rimuovere `src/distribute/typed_process.gleam`
- Usare `gleam/otp/process.Subject(msg)` come tipo base
- Fornire helper per convertire `Pid` → `Subject(msg)` dove necessario
- Aggiornare `messaging`, `registry`, `groups` per usare `process.Subject`

**Verifica**: 
```bash
gleam test
# Tutti i test devono usare process.Subject
```

---

### 2. Creare receiver helpers con Selector integration

**Problema**: Nessun modo type-safe per ricevere messaggi decodificati.

**Azione**: Creare `src/distribute/receiver.gleam` con:

```gleam
import gleam/otp/process.{Selector, Subject}
import distribute/codec

/// Receive and decode a single typed message with envelope validation
pub fn receive_typed(
  decoder: Decoder(a),
  tag: String,
  version: Int,
  timeout: Int,
) -> Result(a, ReceiveError)

/// Add a typed message handler to a Selector
pub fn selecting_typed(
  selector: Selector(msg),
  decoder: Decoder(a),
  tag: String,
  version: Int,
  mapper: fn(a) -> msg,
) -> Selector(msg)

/// Create an actor that receives typed messages
pub fn start_typed_receiver(
  init: fn() -> state,
  handler: fn(state, a) -> state,
  decoder: Decoder(a),
  tag: String,
  version: Int,
) -> Result(Subject(a), ActorError)
```

**Verifica**:
```bash
gleam test
# Test che creano actor, inviano messaggi typed e ricevono correttamente
```

---

### 3. Refactor registry per restituire process.Subject

**Problema**: `registry.whereis` restituisce `Pid` opaco, non `Subject(msg)` typed.

**Azione**: Modificare `src/distribute/registry.gleam`:

```gleam
import gleam/otp/process.{Subject}

/// Register a typed subject globally
pub fn register_typed(name: String, subject: Subject(msg)) -> Result(Nil, RegisterError)

/// Lookup a globally registered typed subject
/// Returns Subject that can be used directly with process.send
pub fn whereis_typed(name: String) -> Result(Subject(msg), RegisterError)
```

**Verifica**:
```bash
gleam test
# Test che registrano e lookup Subject typed
```

---

### 4. Aggiungere actor-based helpers per pattern comuni

**Problema**: Utenti devono costruire actor manually per pattern distribuiti.

**Azione**: Creare `src/distribute/actor.gleam` con pattern comuni:

```gleam
/// Start a request-response actor with typed messages
pub fn start_server(
  init: fn() -> state,
  handler: fn(state, req) -> #(state, resp),
  request_decoder: Decoder(req),
  response_encoder: Encoder(resp),
  tag: String,
  version: Int,
) -> Result(Subject(req), ActorError)

/// Start a worker pool for typed tasks
pub fn start_pool(
  size: Int,
  worker: fn(task) -> result,
  task_decoder: Decoder(task),
  result_encoder: Encoder(result),
  tag: String,
  version: Int,
) -> Result(Subject(task), PoolError)
```

**Verifica**:
```bash
gleam test
# Test che creano server/pool e inviano richieste typed
```

---

### 5. Deprecare/marcare unsafe le API untyped legacy

**Problema**: API untyped ancora esposte senza warning.

**Azione**: Aggiungere deprecation warnings e documentazione chiara:

```gleam
/// ⚠️ UNSAFE: Send untyped message. Use send_typed instead.
/// This function bypasses all type checking and may cause runtime errors.
@deprecated("Use send_typed with proper Encoder instead")
pub fn send(pid: Pid, msg: a) -> Nil

/// ⚠️ UNSAFE: Broadcast untyped message. Use broadcast_typed instead.
@deprecated("Use broadcast_typed with proper Encoder instead")
pub fn broadcast(group: String, msg: a) -> Result(Nil, GroupError)
```

**Verifica**:
```bash
gleam check
# Warning su usi di API deprecated
```

---

### 6. Creare esempi end-to-end completi

**Problema**: Nessun esempio che mostra pattern typed completo.

**Azione**: Creare `examples/typed_hello_world/`:

```
examples/typed_hello_world/
  ├── README.md              # Spiega il pattern
  ├── src/
  │   ├── shared_types.gleam # Codec condivisi tra nodi
  │   ├── server.gleam       # Actor che riceve messaggi typed
  │   └── client.gleam       # Invia messaggi typed al server
  └── scripts/
      └── run.sh             # Avvia entrambi i nodi
```

**Contenuto minimo**:
- Server che riceve `Request` e risponde `Response`
- Codec condivisi in modulo separato
- Client che invia richieste e riceve risposte
- Gestione errori (mismatch tag/version)

**Verifica**:
```bash
cd examples/typed_hello_world
./scripts/run.sh
# Verifica che i messaggi vengano scambiati correttamente
```

---

### 7. Aggiornare README con pattern di integrazione

**Problema**: README ancora menziona "type-safe" senza mostrare come.

**Azione**: Riscrivere sezioni chiave del README:

```markdown
## Type-Safe Distributed Computing

This library provides **true type safety** for distributed Gleam applications through:

1. **Binary encoding with validation**: All messages use `Encoder(a)` / `Decoder(a)`
2. **Envelope versioning**: Tag + version prevent type mismatches
3. **gleam_otp integration**: Uses standard `process.Subject(msg)` 
4. **Compile-time guarantees**: Typed APIs catch errors at compile time

### Quick Start (Typed)

\`\`\`gleam
import gleam/otp/process
import distribute/codec
import distribute/messaging
import distribute/registry

// Define your message type
pub type Greeting {
  Hello(name: String)
}

// Define codecs (shared between nodes)
pub fn greeting_encoder() -> codec.Encoder(Greeting) {
  fn(greeting) {
    case greeting {
      Hello(name) -> codec.encode(codec.string_encoder(), name)
    }
  }
}

// Sender
pub fn send_greeting() {
  let subject = registry.whereis_typed("greeter")
  messaging.send_typed(subject, Hello("Alice"), greeting_encoder())
}

// Receiver (actor)
pub fn start_greeter() {
  receiver.start_typed_receiver(
    init: fn() { Nil },
    handler: fn(_state, greeting) {
      case greeting {
        Hello(name) -> io.println("Hello, " <> name <> "!")
      }
    },
    decoder: greeting_decoder(),
    tag: "greeting",
    version: 1,
  )
}
\`\`\`

### Migration from Untyped APIs

See [MIGRATION.md](MIGRATION.md) for step-by-step guide.
```

**Verifica**:
```bash
# README deve essere chiaro e onesto
```

---

### 8. Creare guida di migrazione dettagliata

**Azione**: Creare `MIGRATION.md`:

```markdown
# Migration Guide: Untyped → Typed APIs

## Why Migrate?

- **Type safety**: Catch errors at compile time
- **Versioning**: Prevent type mismatches across deployments
- **Integration**: Works with gleam_otp ecosystem
- **Maintainability**: Explicit contracts between nodes

## Step-by-Step Migration

### 1. Define Message Types
### 2. Create Codecs
### 3. Replace send → send_typed
### 4. Replace receive loops → typed actors
### 5. Update registry calls
### 6. Test with mismatch scenarios

## Breaking Changes

- `Subject(a)` removed, use `process.Subject(msg)`
- Registry now returns `Subject(msg)` not `Pid`
- Untyped APIs deprecated

## Common Patterns

[esempi pratici]
```

**Verifica**:
```bash
# Guida deve coprire tutti i casi d'uso comuni
```

---

## 🔧 Comandi di sviluppo

```bash
gleam format          # Format code
gleam check           # Type check
gleam test            # Run all tests
gleam build           # Build project
gleam docs build      # Generate docs
```

---

## 🎯 Definition of Done

La libreria può dichiararsi "type-safe reference implementation" quando:

- [x] Usa `gleam/otp/process.Subject(msg)` come tipo base
- [x] Fornisce receiver helpers che integrano con `Selector`
- [x] Registry restituisce `Subject(msg)` typed
- [x] API untyped sono deprecate con warning chiari
- [x] Almeno 3 esempi end-to-end typed completi
- [x] README onesto su cosa è type-safe e cosa no
- [x] Guida di migrazione completa
- [x] 200+ test che coprono anche lato receiver (182 tests currently)
- [x] Documentazione inline completa per tutte le API pubbliche

---

## 🚀 Priorità di implementazione

**Fase 2A (Fondamenta)**: 
- [x] Sostituire Subject custom con process.Subject
- [x] Creare receiver helpers base

**Fase 2B (Ergonomia)**:
- [x] Refactor registry
- [x] Actor helpers per pattern comuni

**Fase 2C (Documentazione)**:
- [x] Deprecare API legacy
- [x] Esempi end-to-end
- [x] README + migration guide

---

## 🛤 Roadmap di rilascio (2.1.0+)
Di seguito un piano di rilascio a piccoli passi — ogni versione introduce miglioramenti mirati, testati e documentati, per facilitare l'espansione tramite adattatori esterni. sempre type-safe e con l'uso corretto delle best practices di gleam (vedi docuemntazione)

### 2.1.0 — Handshake & Negotiation (Fondamentale)
- Implementare `NodeCapabilities` e tipo `Capability(protocol, min, max)`
- Handshake sulla connessione: scambio capability e negoziazione versione
- Memorizzare `negotiated_versions` in `MemberMetadata`
- API pubbliche:
  - `protocol_negotiate(node, protocol) -> Option(Int)`
  - `schema_encode_for_node(schema, value, node)` (utilizza negotiated version)
- Test: unit + integrazione per negoziazione e casi mismatch
- Documentazione e note di rilascio chiare

**Nota sul supporto crittografico (crypto provider)**

Decidiamo di includere nel rilascio 2.1.0 il design e le API core per un "Crypto Provider" pluggabile, ma non tutte le implementazioni concrete. In pratica:

- In 2.1.0: definire il behaviour/trait `crypto.Provider` e la state machine di handshake che include gli stati per KeyExchange (es. `Plain`, `KeyExchangeInProgress`, `SecureEstablished`, `Rekeying`, `Failed`). Fornire hook/API per registrare un provider e per ottenere un `secure_context` opaque che può essere usato da `schema_encode_for_node` / `schema_decode_from_node`.
- In 2.1.0: fornire almeno un'implementazione di riferimento minimale (opzionale) che usa primitive BEAM disponibili, ma documentare chiaramente che è un esempio e non una scelta obbligatoria.
- In 2.2.0: pubblicare implementazioni esempio aggiuntive (AEAD, ECDH key-exchange, rekeying strategies) e una conformance test-suite per provider esterni.

Motivazione: includere il contract/API in 2.1.0 è necessario perché la negoziazione versione e i messaggi di handshake devono prevedere i passi di KeyExchange e le transizioni di stato; lasciare le implementazioni complesse a 2.2.0 riduce il rischio di rallentare il rilascio.

#### Concetti progettuali (estendibilità e sicurezza)

Questa release definisce i concetti base che devono essere generici ed estendibili dagli utilizzatori:

- Envelope generico: ogni messaggio trasmesso usa un envelope `#(tag: String, version: Int, payload: Binary)` che permette di "peek" (leggere tag/version) senza decodificare il payload. API richieste: `envelope.peek_tag(binary) -> Result(#(String, Int), Error)`, `envelope.wrap(tag, version, binary)` e `envelope.unwrap(binary) -> Result(#(String, Int, Binary), Error)`.
- Codec composabili: fornire builder e codec predefiniti (primitives, tuples, Option, Result, custom) oltre a `codec.register_custom(name, Encoder/Decoder)` per estensioni. I codec devono essere indipendenti dalla versione; la mappatura schema→version deve essere gestita dalla layer di negoziazione.
- Messaggi handshake di default: definire tipi e codec per almeno questi messaggi:
  - `Hello(node_info, capabilities)` — inviato dall'initiator con lista capability
  - `Capabilities(capabilities)` — list response
  - `Accept(protocol, version)` — accetta una capability/versione negoziata
  - `Reject(reason)` — rifiuto esplicito con motivo
  I messaggi devono essere estendibili: permettere nuovi message types mantenendo il core minimale.
- Meccanismo di negoziazione: state machine semplice (Initiator → Hello → Capabilities → Accept/Reject → Established) con timeout e rollback. Fornire API pubbliche per intercettare gli eventi (`on_negotiation_success(node, metadata)`, `on_negotiation_failure(node, reason)`).
- Hooks di estensione: API per registrare handler custom per message types e capability validators. Esempio: `handshake.register_handler("my_ext", fn(msg, ctx) -> Result(handled, Error) end)`.
- Retrocompatibilità e migration: mantenere mapping versione→decoder e fornire helper `migrations.apply_chain(value, from_v, to_v)` per supportare trasformazioni in-line. Loggare e fallire in modo chiaro sui mismatch non risolvibili.
- Sicurezza e atom creation: tutte le parti che convertono stringhe in atom devono rispettare `settings.set_allow_atom_creation`. Le API handshake devono usare stringhe/bytes per i tag e lasciare la conversione sotto controllo dell'utente o di helper che rispettano le impostazioni.

Questi concetti vanno poi tradotti in implementazione concreta (codec + helper + test) nelle attività successive della milestone.


### 2.2.0 — Discovery Behaviour & Membership glue
- Aggiungere `discovery/behaviour.gleam` (tipi `Peer`, `PeerEvent`, `Discovery` behaviour)
- Estendere `membership` per consumare eventi Discovery → stato membri
- Creare conformance test suite per adapter discovery
- Fornire uno scheletro adapter `dev/adapters/k8s_stub` per esempi (gli adapter effettivi sono pacchetti esterni)
- Documentare come scrivere un adapter esterno

### 2.3.0 — Adapter Registry & Observability
- Registry runtime per adapter (registrazione, fallback, priorità)
- Metriche ed eventi: `peer_up`, `peer_down`, `negotiation_failed`
- Health-check API per adapter e modalità di fallback automatico
- Integrazione con logging e suggerimenti per production

### 2.4.0 — Example Adapters & Conformance (gli adapter sono repository esterni)
- Implementare adapter di riferimento:
  - `distribute-discovery-k8s` (watcher + pod annotation)
  - `distribute-discovery-dns-srv`
- Fornire test di integrazione end-to-end con cluster simulato
- Checklist di conformance completa per nuovi adapter

### 2.5.0 — Hot-swap & Runtime Configuration
- Supporto per reload dinamico della config discovery (enable/disable adapters)
- Hot-swap degli adapter senza riavviare il nodo
- Miglioramenti UX per fallback e modalità di degradazione

### 2.6.0 — Supervisor foundation, UX & Tooling
- Implement child-spec helpers (non-breaking):
  - `child_spec_membership(opts) -> ChildSpec`
  - `child_spec_discovery(adapter, opts) -> ChildSpec`
  - `child_spec_connection_pool(opts) -> ChildSpec`
- Provide an experimental skeleton module `distribute/supervisor.gleam` that exposes `start_supervisor(opts)` with minimal, opt-in presets (e.g., `one_for_one`) — experimental and opt-in only
- Add examples showing how to integrate child-specs into an existing supervision tree
- Add unit tests for child-spec helpers and basic supervisor skeleton behaviour
- Tooling: generator for adapter scaffold (`distribute gen adapter k8s`), template README for adapters, and deployment examples for Kubernetes/Docker Compose

### 2.7.0 — Supervisor finalization & stability
- Finalize `distribute/supervisor.gleam`: implement restart policies, backoff strategies, `max_restarts`, and presets (`:minimal`, `:full`)
- Add integration tests covering restart policies, adapter failures and recovery
- Document supervisor configuration, recommended policies, and migration notes
- Update `examples/two_nodes_app` to include a full example that uses `start_supervisor` with production-ready presets
- Add changelog/release notes explaining the new optional supervisor module and its usage

### Note di rilascio / compatibilità
- Ogni versione produce un CHANGELOG con breaking note se necessarie
- Backwards compatibility: preferire upgrade non-breaking quando possibile
- Test di upgrade (rolling upgrades) inclusi nella pipeline

---



## 📝 Note su breaking changes

Questa è una **migrazione major breaking**:
- Bump a `2.0.0`
- Comunicare chiaramente nei release notes
- Fornire codemod/script di migrazione se possibile
- Mantenere branch `1.x` per bugfix legacy se necessario

---

## 📝 Note su breaking changes

Questa è una **migrazione major breaking**:
- Bump a `2.0.0`
- Comunicare chiaramente nei release notes
- Fornire codemod/script di migrazione se possibile
- Mantenere branch `1.x` per bugfix legacy se necessario

---

## 📚 Appendice: Piano originale (Fase 1 - Completata)

<details>
<summary>Espandi per vedere i passi della Fase 1</summary>

### Passi completati:

1) ✅ Aggiungere dipendenze gleam_erlang e gleam_otp
2) ✅ Definire behaviour Encoder/Decoder
3) ✅ Introdurre Subject(a) wrapper (da sostituire con process.Subject)
4) ✅ Rifattorizzare messaging.gleam con API typed
5) ✅ Rifattorizzare registry.gleam (parziale, da completare)
6) ✅ Rifattorizzare remote_call.gleam con Decoder
7) ✅ Aggiornare groups.gleam per typed Subjects
8) ✅ Aggiornare FFI Erlang per binari
9) ✅ Test estesi per codec e typed subjects (187 test)

### File creati/modificati:
- `gleam.toml` — dipendenze
- `src/distribute/codec.gleam` — sistema codec
- `src/distribute/typed_process.gleam` — Subject custom (da rimuovere)
- `src/distribute/messaging.gleam` — API typed
- `src/distribute/remote_call.gleam` — RPC typed
- `src/distribute/groups.gleam` — Groups typed
- `src/*_ffi.erl` — FFI aggiornati
- `test/codec_test.gleam` — test codec
- `test/typed_messaging_test.gleam` — test messaging
- `test/typed_process_test.gleam` — test subjects

</details>
