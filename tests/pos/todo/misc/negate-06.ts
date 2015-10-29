/*@ negate :: /\ (x:number)  => {number  | v > x}  
              /\ (x:boolean) => {boolean | true }  */
function negate(x): any {
  if (typeof(x) === "number") {
    return x + 1;
  } 
  else {
    return !x;
  }
}

/*@ main :: () => boolean */
function main() {
	let a = negate(10);
	let b = negate(true);
	return b;
}
