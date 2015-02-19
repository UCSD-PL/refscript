function idt<A>(x:A): A { return x;}

function apply<A,B>(f: (a: A) => B, x: A): B {
	return f(x);
}

/*@ main :: (x:number, boolean) => { v:number |v > x} */
function main(x:number,y:boolean) : number {
	var yr = idt(y);
	var xr = idt(x);
	var z /*@ readonly */ = 1;
  
	function plus(a:number): number {  
		return a + z
	};
	
	xr = apply(plus, xr);
	xr = apply(plus, xr);

	return xr;
}
 


