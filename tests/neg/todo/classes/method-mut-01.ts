class Foo {
  public x:number;

  /*@ new () => void */
  constructor() { this.x = 3; }

  /*@ bar: (): { void | 0 < 1 } */
  bar() { this.x = 4; }
}

/*@ f :: Foo<Mutable> */
var f = new Foo();

f.bar();


