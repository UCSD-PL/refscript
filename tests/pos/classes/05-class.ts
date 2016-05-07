class A<M extends ReadOnly> {
    /*@ a : { number | v > 0 } */
    public a = 2;
    /*@ b : { number | v = 1 } */
    public b = 1;
    /*@ foo(): { number | v > 1 } */
    public foo() {
        return 2;
    }
    constructor() { }
}

class B<M extends ReadOnly> extends A<M> {
    /*@  a : { number | v > 5 } */
    public a = 10;

	/*@ foo(): { number | v > 5 } */
    public foo() {
        return 10;
    }
    constructor() {
		super();
	}
}

let b = new B();
assert(b.a > 5);
assert(b.b === 1);
