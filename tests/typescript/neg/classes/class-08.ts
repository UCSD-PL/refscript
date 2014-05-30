class Foo<A> { 

	/*@ f :: A */ 
	public f;
  
	/*@ constructor :: (x:A) => void */
	constructor(x) {
		this.f = x;
	}
	
}

/*@ p :: { number | v > 0 } */
var p: number;

var a = new Foo(p);


if (a.f > 2) {

  assert(a.f > 3);

}
