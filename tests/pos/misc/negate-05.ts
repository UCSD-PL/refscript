/*@ negate :: /\ ({number | v > 0}) => {number  | v < 0 }  
              /\ (x:boolean)        => {boolean | true  } */
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
	var a:number = negate(10);
	var b:boolean = negate(true);
	return;
}
