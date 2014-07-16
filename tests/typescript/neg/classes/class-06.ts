class Foo<A> { 

	/*@ f : A */ 
	public f;

	/*@ g : A */ 
	public g;
  
	/*@ new(x:A) => void */
	constructor(x: A) {
    this.g = x;
	}
	
}

var a :Foo<number>= new Foo<number>(1);

assert(a.f === 2);
