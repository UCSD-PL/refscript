class Foo {
  public x:number;

  /*@ new () => Foo<M> */
  constructor() { this.x = 3; }

  /*@ bar: (this: Foo<Mutable>): { void | true } */
  bar() { this.x = 4; }
}

/*@ f :: Foo<Mutable> */
var f = new Foo();

f.bar();


