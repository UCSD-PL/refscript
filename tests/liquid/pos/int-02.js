/*@ foo :: (x: {v:  boolean | ttag(v) = "number"} ) => 
            { v: number + boolean + string | ttag(v) = "string" } */

function foo (x) {
  
  return x;
}

