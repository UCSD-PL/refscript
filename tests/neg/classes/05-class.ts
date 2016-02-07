class Foo<A> { 
	public f: A;  
  /*@ new (x: A) => { Foo<M,A> | 0 < 1 } */
	constructor(x: A) {
		this.f = x;
	}	
}

/*@ p :: { number | v > 0 } */
var p: number;

var a = new Foo(p);

if (a.f > 2) {
  assert(a.f > 3);
}
