/*@ negate :: (number|boolean) => number|boolean */
function negate(x) {

//Original - revert when strings are supported   
//  if (typeof(x) == "number") {

  if (typeof(x) == 1) {
    return 0 - x;
  } else {
  }
  return 0;
}

