/*@ negate :: /\ ({number | v > 0}) => {number  | v < 0 }  
              /\ (x:boolean)        => {boolean | 0 < 1 } */
function negate(x): any {
  if (typeof(x) === "number") {
    return 0 - x;
  } 
  else {
    return !x;
  }
}

/*@ main :: () => void */
function main():void{
	let a:number = negate(10);
	let b:boolean = negate(true);
	return;
}
