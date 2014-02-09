
/*@ foo :: () => void */
function foo() {}

class BankAccount { 

  public f /*@ { number | v > 0 }  */ = 1;
  
  public g /*@ { string | v = "a" } */ = "a";
  
  /*@ (x: { string | v = "a"} ) => void */
  constructor(x) {
    this.g = x;  
  }
 
} 

var ba = new BankAccount("a");

ba.g = "b";
