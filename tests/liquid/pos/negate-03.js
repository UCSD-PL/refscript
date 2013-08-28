
/*@ negate :: (x: {number | v > 0} + boolean) =>  { v: number | v < 0 } */

function negate(x) {
  if (typeof(x) == "number") {
    return 0 - x;
  } 
  return -1 ;
  
}

