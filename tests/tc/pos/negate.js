/*@ negate :: (number|boolean) => number */
function negate(x) {

//Original - revert when strings are supported   
  if (typeof(x) == 0) {
    return x;
    // make this work:
    // return 0-x;
  }
  else {
    return 0;
  }
}

