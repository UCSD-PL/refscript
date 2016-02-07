
/*@ class Foo<M,A> */
class Foo<A> { 

	/*@ f : [Mutable] A */ 
	public f;
  
  /*@ new(x:A) => Foo<M,A> */
	constructor(x) {
		this.f = x;
	}
	
}

var a = new Foo(1);

assert(a.f === 1);
