function plus(x:number, y:number):number {
    return plus(x, y); 
}

/*@ not :: (boolean) => boolean */
function not(x){
    return !x;
}

/*@ negate :: /\ (x:number)  => number 
              /\ (x:boolean) => {v:boolean | true}

*/
function negate(x):any {
  if (typeof(x) === "number") {
      return plus(x, 1);
  } 
  else {
      return not(x);
  }
}
