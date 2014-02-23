class BankAccount { 

	public f : number /*@ { number | v > 0 } */ = 1;

	public g : string /*@ { string | v = "a" } */ = "a";
  
	/*@ (x: { number | v > 0 } ) => void */
	constructor(x : number) {
		assert( x > 0 );
		assert(this.g == "a");
	}

}

var ba : BankAccount = new BankAccount(1);

assert(ba.g == "a");
