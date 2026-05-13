import distribute/actor
import distribute/codec
import distribute/global
import distribute/receiver
import distribute/registry
import gleam/erlang/process
import gleeunit
import gleeunit/should

pub fn main() {
  gleeunit.main()
}

pub fn call_fast_fail_test() {
  process.trap_exits(True)
  let tn = registry.named("fail_target", codec.int())

  // 1. Start an actor that we will kill
  let assert Ok(gs) =
    actor.start(tn, 0, fn(_msg, state) { receiver.Continue(state) }, 1000)

  let assert Ok(pid) = process.subject_owner(global.subject(gs))

  // 2. Prepare a call to this actor
  // We'll launch it in a separate process so we can kill the actor mid-call
  let res_subject = process.new_subject()
  process.spawn(fn() {
    let res = global.call(gs, fn(_reply_subj) { 42 }, codec.int_decoder(), 5000)
    process.send(res_subject, res)
  })

  // 3. Kill the actor
  process.sleep(50)
  // ensure call has started and monitor is established
  process.kill(pid)

  // 4. Verification: call should fail with TargetDown immediately (not wait 5s)
  let assert Ok(res) = process.receive(res_subject, 1000)
  should.equal(res, Error(global.TargetDown))
}

pub fn call_timeout_test() {
  let tn = registry.named("slow_target", codec.int())

  // Actor that ignores messages (never replies)
  let assert Ok(gs) =
    actor.start(tn, 0, fn(_msg, state) { receiver.Continue(state) }, 1000)

  let res =
    global.call(
      gs,
      fn(_reply_subj) { 42 },
      codec.int_decoder(),
      100,
      // short timeout
    )

  should.equal(res, Error(global.Timeout))

  process.send_exit(process.subject_owner(global.subject(gs)) |> should.be_ok())
}
