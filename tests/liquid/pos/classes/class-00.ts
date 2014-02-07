var a /*@ { number | v > 0 } */ = 1;

/*@ foo :: () => void */
function foo() {}

class BankAccount { 

  public a /*@ { number | v > 0 } */ = 1;

  public b /*@ { string | v = "a" } */ = "a";
  
  /*@ (a: { number | v = 1} ) => void */
  constructor(a) {
    assert( a + 1 == 2);
    assert(this.b == "a");
  }

}

var ba = new BankAccount(100);
