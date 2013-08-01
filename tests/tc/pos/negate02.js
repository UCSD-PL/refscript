/*@ negate :: (number + boolean) => number */
function negate(x) {

  //Revert when strings are supported   
  if (typeof(x) == 1) {
    if (typeof(x) == 0) 
      return 0-x;
    else 
      return 0;
  }
  else if (typeof(x) == 1) {
    return !x;
  }
}

/*@ main :: (number) => void */ 
function main(x) {

  negate(x);

}
