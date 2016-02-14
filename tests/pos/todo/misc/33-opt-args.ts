//adapted from richards-typed-octane
class Task {
    
    /*@ foo : (packet: Packet<Mutable>) : { void | 0 < 1} */
    /*@ foo : () : { void | 0 < 1} */
    foo(packet?) {
        if (packet) {
          (<Packet>packet).id = 0;
        }
    }

    /*@ bar : (packet: Packet<Mutable> + undefined) : { void | 0 < 1} */
    bar(packet?) {
        if (packet) {
          (<Packet>packet).id = 0;
        }
    }
    constructor() {}
}
declare class Packet {
public id;
}
