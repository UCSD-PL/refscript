
/*@ negate :: (x: {number | v > 0} + boolean) =>  { v: number | v < 0 } */

function negate(x):any {
  if (typeof(x) === "number") {
    return 0 - <number>x;
  }
  return -1 ;

}
