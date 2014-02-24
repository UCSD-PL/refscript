/*@ idt :: forall A. (A) => A */
function idt(x:any):any { return x;}

/*@ twice :: forall A. ((A) => A, A) => A */
function twice(f:(any)=>any, x0:any):any{
	var x1 :any= f(x0);
	var x1 :any= f(x1);
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



