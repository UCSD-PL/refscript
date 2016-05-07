
function forloop<A>(lo : number, hi : number, body : (x: number, y: A) => A, acc : A): A {
	if (lo < hi) {
		let newAcc = body(lo, acc);
		return forloop(lo + 1, hi, body, newAcc);
	}
	return acc;
}

function plus(i: number, x: number) {
	return x + i;
}

/*@ main :: ({n:number| n > 0}) => void */
export function main(n) : void {
  	let m = forloop(0, n, plus, n);
  	assert(m >= n);
}
