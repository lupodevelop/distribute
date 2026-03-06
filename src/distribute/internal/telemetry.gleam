import gleam/dict.{type Dict}
import gleam/dynamic.{type Dynamic}

/// Bridge to Erlang's :telemetry library. Internal use only.
@external(erlang, "telemetry", "execute")
fn erlang_execute(
  event_name: List(Dynamic),
  measurements: Dict(Dynamic, Dynamic),
  metadata: Dict(Dynamic, Dynamic),
) -> Nil

@external(erlang, "distribute_ffi_utils", "system_time_ms")
pub fn system_time() -> Int

@external(erlang, "distribute_ffi_utils", "to_atom_safe")
fn to_atom(name: String) -> Dynamic

// -- Factory Helpers --------------------------------------------------------

fn emit(
  path: List(String),
  measurements: List(#(String, Dynamic)),
  metadata: List(#(String, Dynamic)),
) -> Nil {
  erlang_execute(
    list_to_atoms(path),
    dict.from_list(list_to_atom_pairs(measurements)),
    dict.from_list(list_to_atom_pairs(metadata)),
  )
}

fn list_to_atoms(list: List(String)) -> List(Dynamic) {
  case list {
    [] -> []
    [first, ..rest] -> [to_atom(first), ..list_to_atoms(rest)]
  }
}

fn list_to_atom_pairs(
  list: List(#(String, Dynamic)),
) -> List(#(Dynamic, Dynamic)) {
  case list {
    [] -> []
    [#(k, v), ..rest] -> [#(to_atom(k), v), ..list_to_atom_pairs(rest)]
  }
}

// -- Domain Events ----------------------------------------------------------

pub fn emit_message_send_stop(
  duration: Int,
  size: Int,
  target: String,
  success: Bool,
) -> Nil {
  emit(
    ["distribute", "message", "send", "stop"],
    [#("duration", dynamic.int(duration)), #("size", dynamic.int(size))],
    [#("target", dynamic.string(target)), #("success", dynamic.bool(success))],
  )
}

pub fn emit_registry_lookup_stop(
  name: String,
  found: Bool,
  duration: Int,
) -> Nil {
  emit(
    ["distribute", "registry", "lookup", "stop"],
    [#("duration", dynamic.int(duration))],
    [#("name", dynamic.string(name)), #("found", dynamic.bool(found))],
  )
}

pub fn emit_registry_register_stop(
  name: String,
  success: Bool,
  duration: Int,
) -> Nil {
  emit(
    ["distribute", "registry", "register", "stop"],
    [#("duration", dynamic.int(duration))],
    [#("name", dynamic.string(name)), #("success", dynamic.bool(success))],
  )
}

pub fn emit_codec_encode_stop(duration: Int, size: Int, success: Bool) -> Nil {
  emit(
    ["distribute", "codec", "encode", "stop"],
    [#("duration", dynamic.int(duration)), #("size", dynamic.int(size))],
    [#("success", dynamic.bool(success))],
  )
}

pub fn emit_codec_decode_stop(duration: Int, success: Bool) -> Nil {
  emit(
    ["distribute", "codec", "decode", "stop"],
    [#("duration", dynamic.int(duration))],
    [#("success", dynamic.bool(success))],
  )
}

pub fn emit_node_event(node: String, type_name: String) -> Nil {
  emit(["distribute", "cluster", "node_event"], [#("count", dynamic.int(1))], [
    #("node", dynamic.string(node)),
    #("type", to_atom(type_name)),
  ])
}
