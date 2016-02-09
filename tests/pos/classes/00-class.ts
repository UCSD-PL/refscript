class BankAccount<M extends ReadOnly> {

	/*@ f: posint */
	public f : number = 1;

	/*@  g : { string | v = "a" } */
	public g: string = "a";

  /*@ new (x: posint): BankAccount<M> */
	constructor(x : number) {
		assert( x > 0 );
		// assert(this.g === "a");
	}

}

let ba = new BankAccount(1);

assert(ba.g === "a");
