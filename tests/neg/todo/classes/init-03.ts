
class Foo {

  /*@ f : { number | v = 4 } */
  public f: number;

  /*@ new (id: { number | v = 4 }) => void */
  constructor(a: number) { this.f = a; }

}

/*@ createFoo :: () => { Foo<Immutable> | 0 < 1 } */
function createFoo() {
  /*@ foo :: Foo<UniqueMutable> */
  var foo = new Foo(4);
  foo.f = 10;
  return foo;
}

