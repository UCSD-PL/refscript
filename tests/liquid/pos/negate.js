/*@ negate :: (x: number + boolean) => { v: number + boolean | true } */
function negate(x) {
  if (typeof(x) == "number") {
    return 0 - x;
  } else {
    return !x;
  }
}

