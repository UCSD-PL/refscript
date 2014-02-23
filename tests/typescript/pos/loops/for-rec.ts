/*@ forloop :: forall A. (number, number, (number, A) => A, A) => A */
function forloop(lo : number, hi : number, body : (number,any)=>any, acc : any) :any{
	if (lo < hi) {
		var newAcc : any = body(lo, acc);
		return forloop(lo + 1, hi, body, newAcc);
	}
	return acc;
}

/*@ plus :: (number, number) => number */
function plus(i:number, x:number) :number { return x + i ; }

/*@ main :: ({n:number| n > 0}) => void */
function main(n) : void {
  var m = forloop(0, n, plus, n);
  assert(m >= n);
}
