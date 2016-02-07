//adapted from transducers

interface Foo {}

class FooImpl implements Foo {
/*@ new() => { Foo<M> | 0 < 1} */
  constructor() { }
}

/*@ x :: Foo<Immutable> */
var x = new FooImpl();
