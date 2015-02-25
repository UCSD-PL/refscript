
class Foo<A> { 
	public f:A;
	constructor(x: A) { this.f = x; }
}

var a = new Foo(1);

assert(a.f === 2);
