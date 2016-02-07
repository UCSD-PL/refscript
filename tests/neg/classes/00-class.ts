
class BankAccount<M extends ReadOnly> {
	/*@ f: posint */
	public f = 1;
	/*@ g: { string | v = "a" } */
	public g = "a";

	constructor(x) {
		this.g = x;
	}
}

let ba = new BankAccount("a");



ba.g = "b";
