/*@ negate :: (number|boolean) => number */
function negate(n) {

  assert(false);

//Original - revert when strings are supported   
  //if (typeof(x) == 0) {
  //  return 0-x;
  //}
  //else {
  //  return !x;
  //}
  return 1;
}

/*@ main :: ({v:number|v > 0}, {v:number|v >0}) => void */ 
function main(x,y) {

  negate(x);

  assert(false);

}
