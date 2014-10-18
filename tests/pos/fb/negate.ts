/*@ negate :: /\ (x:number)  => number
              /\ (x:boolean) => boolean */

function negate(x):any {
  if (typeof(x) === "number") {
    return 0 - x;
  } 
  else {
    return !x;
  }
}


/*@ a :: number */
var a = negate(true);

/*@ b :: boolean */
var b = negate(true);
