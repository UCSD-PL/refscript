class BankAccount { 

  public f /*@ { number | v > 0 } */ = 1;

  public g /*@ { string | v = "a" } */ = "a";
  
  /*@ (x: { number | v > 0 } ) => void */
  constructor(x) {
    assert( x > 0 );
    assert(this.g == "a");
  }

}

var ba = new BankAccount(1);

assert(ba.g == "a");
