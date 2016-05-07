// adapted from transducers

class Foo<M extends ReadOnly, T> { constructor() {}}
class Sub<M extends ReadOnly> extends Super<M, Foo<Mutable, number>> { constructor() { super(); } }
let x: Super<Immutable, Foo<Mutable, number>> = new Sub();

class Super<M extends ReadOnly, T> { constructor() {} }
class Sub2<M extends ReadOnly> extends Super<M, Array<Immutable, number>> { constructor() { super(); }}

let y: Super<Immutable, Array<Immutable, number>> = new Sub2();
