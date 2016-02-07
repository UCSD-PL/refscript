
class Foo {
  public f: number;
  /*@ new (id: number) => { void | 0 < 1 } */
  constructor(a: number) { this.f = a; }
}

/*@ createFoo :: () => { Foo<Immutable> | 0 < 1 } */
function createFoo() {
  /*@ foo :: Foo<UniqueMutable> */
  var foo = new Foo(5);
  foo.f = 10;
  return foo;
}

var foo = createFoo();

if (foo.f === 10) {
  assert(foo.f === 100); 
}

