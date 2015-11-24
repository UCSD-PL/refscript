/*@ negate :: /\ (x:number)  => {number  | v > x}  
              /\ (x:boolean) => {boolean | 0 < 1 }  */
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
	var a = negate(10);
	var b = negate(true);
	return b;
}
