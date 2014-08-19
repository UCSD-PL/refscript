function plus(x:number, y:number):number {
    return plus(x, y); 
}

/*@ nein :: (boolean) => {boolean | true} */
function nein(x){
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
      return nein(x);
  }
}

/*@ foo :: (number) => {v:number | v = 1} */
function foo(x:number):any {
    return 1;
    return "cat";
}