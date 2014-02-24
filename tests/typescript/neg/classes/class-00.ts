
/*@ foo :: () => void */
function foo():void {}

class BankAccount { 
	/*@ f :: { number | v > 0 }  */
	public f = 1;
	/*@ g :: { string | v = "a" } */ 
	public g = "a";
	
	/*@ constructor :: (x: { string | v = "a"} ) => void */
	constructor(x) {
    this.g = x;  
  }
 
} 

var ba :BankAccount= new BankAccount("a");

ba.g = "b";
