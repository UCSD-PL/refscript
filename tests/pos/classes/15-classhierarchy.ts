//adapted from transducers

class Super<M extends ReadOnly> {
    constructor() { }
}

class Sub<M extends ReadOnly> extends Super<M> {
    constructor() {
        super();
    }
}

/*@ x :: {Sub<Immutable> | extends_class (v,"Super")} */
let x = new Sub();
