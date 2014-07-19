class A {

	/*@ new () => void */
	constructor() { }
	/*@ a : { number | v > 0 } */
	public a = 1;

	/*@ b : { number | v = 1 } */
	public b = 1;

	/*@ foo : (): { number | v > 1 } */
	public foo() {
		return 2;  
	}

}

class B extends A {

	/*@ new() => void */
	constructor() {super(); }
	/*@  a : { number | v > 5 } */ 
	public a = 10;

	/*@ foo : (): { number | v > 5 } */
	public foo() {
		return 10;  
	}

}

var b = new B();

assert(b.a > 5);

assert(b.b === 1);
