//adapted from richards-typed-octane
class Task<M extends ReadOnly> {

    /*@ foo(packet: Packet<Mutable>) : { void | 0 < 1} */
    /*@ foo(): { void | 0 < 1} */
    foo(packet?) {
        if (packet) {
          (<Packet<Mutable>>packet).id = 0;
        }
    }

    /*@ bar(packet: Packet<Mutable> + undefined) : { void | 0 < 1 } */
    bar(packet?) {
        if (packet) {
          (<Packet<Mutable>>packet).id = 0;
        }
    }
    constructor() {}
}

class Packet<M extends ReadOnly> {

    /*@ id: number */
    public id: number = 1;

    constructor() {}
}
