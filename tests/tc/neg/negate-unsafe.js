/*@ negate :: (number + boolean) => number + boolean */
function negate(x) {
  if (typeof(x) == "number") {
    return !x;
  } else {
    return 0 - x;
  }
}

