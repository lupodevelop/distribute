/// Shared test helpers for distribute tests.
///
/// Provides a unique ID generator for test isolation so each test
/// uses a unique name and won't collide with other tests.
@external(erlang, "erlang", "unique_integer")
fn erlang_unique_int() -> Int

@external(erlang, "erlang", "integer_to_binary")
fn int_to_string(n: Int) -> String

/// Generate a unique string ID suitable for use in test names.
pub fn unique_id() -> String {
  let n = erlang_unique_int()
  int_to_string(case n < 0 {
    True -> 0 - n
    False -> n
  })
}
