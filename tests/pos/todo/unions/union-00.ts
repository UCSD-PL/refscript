
/*@ foo ::  (number + boolean) => number + { string | true } */
function foo(x) {

  if (typeof x === "number") 
    return x;
  return "a";
}

