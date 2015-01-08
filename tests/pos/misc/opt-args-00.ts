//adapted from richards-typed-octane
class Task {
  /*@ foo : /\ (packet: Packet<Mutable>) : { void | true }
    /\ () : { void | true } */
  foo(packet?) {
    if (packet) {
      (<Packet>packet).id = 0;
    }
  }
  /*@ bar : (packet: Packet<Mutable> + undefined) : { void | true } */
  bar(packet?) {
    if (packet) {
      (<Packet>packet).id = 0;
    }
  }
}
declare class Packet {
  public id;
}
