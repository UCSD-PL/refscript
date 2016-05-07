
class BankAccount<M extends ReadOnly> {
	/*@ f: posint */
	public f = 1;
	/*@ g: { string | v = "a" } */
	public g = "a";

	/*@ new (x: any): BankAccount<Unique> */
	constructor(x) {
		this.g = x;
	}
}

let ba = new BankAccount("a");
ba.g = "b";
