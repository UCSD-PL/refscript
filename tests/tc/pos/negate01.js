/*@ negate :: (number + boolean) => number + boolean */
function negate(x) {
//Int   -> 0
//Bool  -> 1
//Revert when strings are supported   
  if (typeof(x) == 0) {
    return 0-x;
  }
  else {
    return !x;
  }
}

/*@ main :: (number + boolean) => void */ 
function main(n) {
  negate(n);
}
