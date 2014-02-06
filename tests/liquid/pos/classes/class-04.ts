class A {

  /*@ () => void */
  constructor() { }

  /*@    a :: { number | v > 0 } */
  public a = 10;

  /*@ foo :: () => { number | v > 1 } */
  public foo() {
    return 2;  
  }

}

class B extends A {

  /*@ () => void */
  constructor() { }

  /*@    b :: { number | v > 5 } */
  public b = 10;

  /*@ foo :: () => { number | v > 5 } */
  public foo() {
    return 10;  
  }

}
