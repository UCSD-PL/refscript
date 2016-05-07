//TODO: shuffle refinements up and down the unions. HASSLE.

/*@ predicate eqtag(x, y) = ttag(x) = ttag(y) */

/*@ negate :: (x: number + boolean) => 
    { v: number + boolean | eqtag(v, x) } */

// The sig below works, natch. 
/* negate :: /\ (x: number)  => {v:number  | eqtag(v, x)}
              /\ (x: boolean) => {v:boolean | eqtag(v, x)} */

function negate(x): any {
  if (typeof(x) === "number") {
    return 0 - x;
  } 
  else {
    return !x;
  }
}

