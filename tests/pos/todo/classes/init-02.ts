
class Foo {
  public f: number;
  /*@ new (id: number) => { Foo<M> | true } */
  constructor(a: number) { this.f = a; }
}

/*@ createFoo :: () => { Foo<Immutable> | true } */
function createFoo() {
  /*@ foo :: Foo<UniqueMutable> */
  var foo = new Foo(5);
  foo.f = 10;
  return foo;
}

var foo = createFoo();

if (foo.f === 10) {
  assert(foo.f === 10); 
}

