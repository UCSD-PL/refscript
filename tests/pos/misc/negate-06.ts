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

/*@ main :: () => void */
function main():void{
	var a:number = negate(10);
	var b:boolean = negate(true);
	return;
}
