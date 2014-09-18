

/*@ negate :: (x: {v: number | v > 0} + boolean) =>  { v: number | v > 0 } + boolean */

function negate(x): any {
  return (typeof(x) === "number") ? (0-x) : (!x);  
}

