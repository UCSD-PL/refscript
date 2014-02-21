class A {

  /*@ () => void */
  constructor() { }

  public a /*@ { number | v > 0 } */ = 1;

  public b /*@ { number | v = 1 } */ = 1;

  /*@ () => { number | v > 1 } */
  public foo() {
    return 2;  
  }

}

class B extends A {

  /*@ () => void */
  constructor() { }

  public a /*@  { number | v > 5 } */ = 10;

  /*@ () => { number | v > 5 } */
  public foo() {
    return 10;  
  }

}

var b = new B();

assert(b.a > 5);

assert(b.b == 1);
