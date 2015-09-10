
class BankAccount { 
	/*@ f : { number | v > 0 }  */
	public f = 1;
	/*@ g : { string | v = "a" } */ 
	public g = "a";
	
	constructor(x) { this.g = x; }
} 

var ba :BankAccount= new BankAccount("a");

ba.g = "b";
