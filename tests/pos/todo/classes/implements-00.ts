//adapted from transducers

interface Foo {}

class FooImpl implements Foo {
/*@ new() => { Foo<M> | true} */
  constructor() { }
}

/*@ x :: Foo<Immutable> */
var x = new FooImpl();
