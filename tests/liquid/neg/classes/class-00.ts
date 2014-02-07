var a /*@ { number | v > 0 } */ = 1;

/*@ foo :: () => void */
function foo() {}

class BankAccount { 

  public a /*@ { number | v > 0 }  */ = 0;
  
  /*@ (a: { number | v = 1} ) => void */
  constructor(a) {

    assert( a + 1 == 1);
  
  }

 
} 
