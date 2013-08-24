/*@ foo :: (x: {v:  boolean | ttag(v) = "number"} ) => { v: number + boolean + string | ttag(v) = "number" } */

function foo (x) {
  
  return x;
}

