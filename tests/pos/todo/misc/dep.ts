
/*@ foo :: (x:number, {y:number | x < y}) => number */
function foo(x:number, y:number) : number{
  return 10;
}

/*@ baz :: ({a:number|true}, {b:number | true}) => number */
function baz(a :number, b:number){
	let r :number= 0;
	if (a < b){
		r = foo(a, b);
	}
	return r;
}
