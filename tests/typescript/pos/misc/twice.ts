
/*@ twice :: forall A. ((A) => A, A) => A */
function twice(f:(any)=>any, x0:any):any{
	var x1 :any= f(x0);
	var x1 :any= f(x1);
	return x1;
}

/*@ foo :: (x:number) => {v:number | v >= x} */
function foo(x:number){
	var z  :number= 0;
  if (random() > 0) {
    z = 10;
  }
	var r :number= x + z;
  assert(r >= x);
  return r;
}
 
/*@ main :: (x:number) => {v:number |v >= x} */
function main(x:number):number{
  return twice(foo, x);
}
