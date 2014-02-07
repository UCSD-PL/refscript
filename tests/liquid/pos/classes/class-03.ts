class A {

  /*@    a :: { number | v > 0 } */
  public a = 10;

  /*@ () => { number | v > 1 } */
  public foo() {
    return 2;  
  }

}
