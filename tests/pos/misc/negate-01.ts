
/*@ negate :: (x: {v: number | v > 0} + boolean) =>  { v: number | v < 0 } + boolean */

function negate(x):any {
  if (typeof(x) === "number") {
    return 0-x;
  } 
  else {
    return !x
  }
  
}

