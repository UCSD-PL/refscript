function idt<A>(x: A): A { return x;}

function twice<A>(f:(a:A)=>A, x0:A): A{
	var x1 = f(x0);
	x1 = f(x1);
	return x1;
}

/*@ main :: (x:number, boolean) => { v:number |v >= x} */
function main(x:number,y:boolean):number{
	var yr :boolean= idt(y);
	var xr :number= idt(x);
	var z  :number= 1;
	if (yr) {
		z = 10;
	}
	
	assert (z > 0);

	/*@ plus :: (number) => number */
	function plus(a:number):number{ return a + z };

	return twice(plus, xr);
}



