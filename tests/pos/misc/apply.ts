function idt<A>(x:A): A { return x;}

function apply<A,B>(f: (a: A) => B, x: A): B {
	return f(x);
}

/*@ main :: (x:number, boolean) => { v:number |v > x} */
function main(x:number,y:boolean) : number {
	var yr = idt(y);
	var xr = idt(x);
  
	function plus(a:number): number {  
		return a + 1;
	};
	
	xr = apply(plus, xr);
	xr = apply(plus, xr);

	return xr;
}
 


