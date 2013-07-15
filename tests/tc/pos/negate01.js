/*@ negate :: (number|boolean, {v:number|v>0}) => number|boolean */
function negate(www,y) {

//Original - revert when strings are supported   
  if (typeof(www) == 0) {
    return 0-www;
  }
  //else {
  //  return !www;
  //}
  return 0;
}

/*@ main :: (number|boolean, {v:number|v>0}) => void */ 
function main(x,y) {
  negate(x,y);

}
