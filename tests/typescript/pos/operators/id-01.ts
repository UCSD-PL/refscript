/*@ id2 :: forall A B. (A, B) => A */
function id2(x:any, y:any):any { return x;}

/*@ main :: ({x:number|true}, boolean) => {v:number|v >= x} */
function main(x:number, y:boolean){
	var yr:boolean = id2(y, x);
	var xr:number = id2(x, y);
	var z:number  = 0;
	if (yr) {
		z = 10;
	}
	return xr + z;
}
  
