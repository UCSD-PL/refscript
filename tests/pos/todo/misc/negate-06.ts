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
	var a = negate(10);
	var b = negate(true);
	return b;
}
