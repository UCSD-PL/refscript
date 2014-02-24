class Foo<A> { 

	/*@ f :: A */ 
	public f;
  
	/*@ constructor :: (x:A) => void */
	constructor(x) {
		this.f = x;
	}
	
}

var a :Foo<number>= new Foo<number>(1);

assert(a.f == 1);
