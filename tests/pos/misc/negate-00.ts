/*@ negate :: (x: number + boolean) =>  
    { v: number + boolean | (ttag(v) = ttag(x)) } */
function negate(x): any {
  if (typeof(x) === "number") {
    return 0 - x;
  } 
  else {
    return !x;
  }
}

