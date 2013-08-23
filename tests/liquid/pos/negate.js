/*@ qualif Tag1(v:a, w:b)               : ttag([v])  = ttag([w])                 */
/*@ qualif Tag2(v:a, w:b)               : ttag([v]) != ttag([w])                 */

/*@ negate :: ({x: number + boolean | true }) => 
    { v: number + boolean | (ttag(v) = ttag(x)) } */
function negate(x) {
  if (typeof(x) == "number") {
    return 0 - x;
  } else {
    return !x;
  }
}

