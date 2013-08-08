/*@ negate :: (number + boolean) => number + boolean */
function negate(x) {
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
