import distribute/codec
import distribute/global
import gleam/erlang/process
import gleeunit
import gleeunit/should

pub fn main() {
  gleeunit.main()
}

pub fn global_subject_send_receive_test() {
  // Use from_subject with a unique subject for test isolation
  let subj = process.new_subject()
  let gs =
    global.from_subject(subj, codec.string_encoder(), codec.string_decoder())
  let assert Ok(Nil) = global.send(gs, "hello global")
  let assert Ok(msg) = global.receive(gs, 1000)
  should.equal(msg, "hello global")
}

pub fn global_subject_from_subject_test() {
  // Create a GlobalSubject from an existing Subject(BitArray)
  let subj = process.new_subject()
  let gs = global.from_subject(subj, codec.int_encoder(), codec.int_decoder())
  let assert Ok(Nil) = global.send(gs, 42)
  let assert Ok(val) = global.receive(gs, 1000)
  should.equal(val, 42)
}

pub fn global_subject_owner_test() {
  let gs = global.new(codec.string_encoder(), codec.string_decoder())
  let assert Ok(pid) = global.owner(gs)
  should.equal(pid, process.self())
}

pub fn global_subject_timeout_test() {
  // Use from_subject with a unique subject for test isolation
  let subj = process.new_subject()
  let gs =
    global.from_subject(subj, codec.string_encoder(), codec.string_decoder())
  // No message sent — should timeout
  let result = global.receive(gs, 10)
  should.be_error(result)
}

pub fn global_subject_encoder_decoder_access_test() {
  let enc = codec.int_encoder()
  let dec = codec.int_decoder()
  let gs = global.new(enc, dec)
  // Verify encoder/decoder are accessible
  let assert Ok(bin) = codec.encode(global.encoder(gs), 99)
  let assert Ok(val) = codec.decode(global.decoder(gs), bin)
  should.equal(val, 99)
}

pub fn global_new_unique_tags_test() {
  // Two global.new() calls should produce isolated subjects
  let gs1 = global.new(codec.int_encoder(), codec.int_decoder())
  let gs2 = global.new(codec.int_encoder(), codec.int_decoder())

  let assert Ok(Nil) = global.send(gs1, 111)
  let assert Ok(Nil) = global.send(gs2, 222)

  // Each receives only its own message
  let assert Ok(v1) = global.receive(gs1, 1000)
  let assert Ok(v2) = global.receive(gs2, 1000)
  should.equal(v1, 111)
  should.equal(v2, 222)
}
