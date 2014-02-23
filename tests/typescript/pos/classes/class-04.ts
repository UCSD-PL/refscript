class A {

	/*@ () => void */
	constructor() { }

	public a /*@ { number | v > 0 } */ = 10;

	/*@ () => { number | v > 1 } */
	public foo() {
		return 2;  
	}

}

class B extends A {

	/*@ () => void */
	constructor() {super(); }

	public b /*@ { number | v > 5 } */ = 10;

	/*@ ( ) => { number | v > 5 } */
	public foo() {
		return 10;  
	}

}
