
class Foo<A> { 
	public f:A;

  /*@ new (x: A) => { Foo<M,A> | 0 < 1 } */
	constructor(x: A) { this.f = x; }
}

var a = new Foo(1);

assert(a.f === 2);
