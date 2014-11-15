class Foo {
  public x:number;

  /*@ new () => void */
  constructor() { this.x = 3; }

  /*@ bar: (this: Foo<Mutable>): { void | true } */
  bar() { this.x = 4; }
}

/*@ f :: Foo<Immutable> */
var f = new Foo();

f.bar();


