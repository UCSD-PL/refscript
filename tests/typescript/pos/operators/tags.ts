/*@ moo :: (number) => {v:string | v = "number"} */
function moo(x:number):string {
	var z :string= typeof(x);
	return z;
}
