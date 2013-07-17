/*@ negate :: (number) => number */
function negate(x) {

//Original - revert when strings are supported   
  if (typeof(x) == 0) {
    return 0-x;
  }
  else {
    return !x;
  }
}

/*@ main :: (number, {v:number|v>0}) => void */ 
function main(x,y) {

  negate(x);

}
