/*@ negate :: (x: number + boolean) 
           => { v: number + boolean | (ttag(v) = ttag(x)) } */
function negate(x: any): any {
  if (typeof(x) == "number") {
    if (x > 100)
      return 0 - x;
    else 
      return false;
  } else {
    return !x;
  }
}

