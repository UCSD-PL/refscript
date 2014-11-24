//adapted from transducers

interface Foo {}

class FooImpl implements Foo {
  /*@ new() => {void | true} */
  constructor() { }
}

/*@ x :: Foo<Immutable> */
var x = new FooImpl();
