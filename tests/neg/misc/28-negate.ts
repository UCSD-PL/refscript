/*@ negate :: (x:number)  => {number  | v > x}  */
/*@ negate :: (x:boolean) => {boolean | 0 < 1 }  */
function negate(x): any {
  if (typeof(x) === "number") {
    return !x;
  }
  else {
    return x + 1;
  }
}

/*@ main :: () => boolean */
function main() {
	let a = negate(10);
	let b = negate(true);
	return b;
}
