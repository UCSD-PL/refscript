// adapted from transducers
declare class Foo<T> {}
declare class Super<T> {}

/*@ class Sub<M> extends Super<M, Foo<Mutable, number>> */
declare class Sub extends Super<Foo<number>> {}

/*@ x :: Super<Immutable, Foo<Mutable, number>> */
var x = new Sub();

/*@ class Sub2<M> extends Super<M, Array<Immutable, number>> */
declare class Sub2 implements Super<Array<number>> {}

/*@ y :: Super<Immutable, Array<Immutable, number>> */
var y = new Sub2();
