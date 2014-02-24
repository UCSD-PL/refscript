/*@ idt :: forall A. (A) => A */
function idt(x:any):any { return x;}


/*@ idbool :: (boolean) => boolean */
function idbool(x:boolean):boolean { return idt(x); }

/*@ main :: ({x:number|true}, boolean) => {v:number|v = x} */
function main(x:number, y:boolean){
	var yr :boolean= idt(y);
	var xr :number= idt(x);
	var z  :number= 0;
	if (yr) {
		z = 10;
		return xr;
	}
	return xr + z;
}


