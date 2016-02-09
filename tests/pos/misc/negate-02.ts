
/*@ negate :: (xxx: {number | v > 0} + boolean) =>  { v: number | v < 0 } + boolean */

function negate(xxx):any {
  if (typeof(xxx) === "number") {
    return 0 - <number>xxx;
  }
  return 0 - 1;

}
