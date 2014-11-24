//adapted from transducers
declare class Foo {}
declare class Super<T> {}
declare class Sub extends Super<Foo> {}
/*@ x :: Super<Immutable, Foo<Immutable>> */
var x = new Sub();
