class BankAccount { 
	/*@ f : { number | v > 0 } */ 
	public f : number = 1;
	/*@ g : { string | v = "a" } */
	public g : string = "a";
  
	/*@ new (x: { number | v > 0 } ) => void */
	constructor(x : number) {
		assert( x > 0 );
		assert(this.g === "a");
	}

}

var ba = new BankAccount(1);

assert(ba.g === "a");
