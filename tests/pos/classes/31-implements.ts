//adapted from transducers

interface Foo { y: number }

interface Bar { x: number }

class FooImpl implements Foo, Bar {

  x = 1;
  y = 2;

  /*@ new() => { FooImpl<M> | 0 < 1} */
  constructor() { }
}

/*@ x :: Foo<Immutable> */
var x = new FooImpl();

/*@ y :: Bar<Immutable> */
var y = new FooImpl();
