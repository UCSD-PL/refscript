
/*@ foo ::  (number + boolean) => number + { string | 0 < 1 } */
function foo(x) {

  if (typeof x === "number") 
    return x;
  return "a";
}

