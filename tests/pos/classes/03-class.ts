
class Foo<M extends ReadOnly, A> {

	/*@ (Mutable) ffffff:  A */
	public ffffff: A;

  	constructor(x: A) {
		this.ffffff = x;
	}

}

let aaa = new Foo(1);

assert(aaa.ffffff === 1);
