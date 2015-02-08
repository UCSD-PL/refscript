
class Foo<A> { 
	public g;
  constructor(x: A) { this.g = x; }
}

var a = new Foo(1);

assert(a.g === 2);
