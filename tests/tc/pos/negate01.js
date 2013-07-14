/*@ negate :: (number|boolean) => number|boolean */
function negate(x) {

//Original - revert when strings are supported   
  if (typeof(x) == 0) {
    return 0-x;
  }
  return 0;
  //else {
  //  return !x;
  //}
}

/*@ main :: (number|boolean, {v:number|v>0}) => void */ 
function main(x,y) {

  negate(x);

}
