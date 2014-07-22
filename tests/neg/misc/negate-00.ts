/*@ negate :: (x: number + boolean) => 
    { v: number + boolean | (ttag(v) = ttag(x)) } */
function negate(x): any {
  if (typeof(x) === "number") {
    return !x;
  } else {
    return 0 - x;
  }
}

