
/*@ qualif Plus(v:number, x:number, y:number)   : v = x + y    */

/*@ sumLoop :: (number, number) => number */
function sumLoop(acc, i){
  var r = acc;
  if (0 < i){
    r = sumLoop(acc + 1, i - 1);
  }  
  return r;
}

/*@ main :: () => {void | true} */
function main(){
  var n = pos();
  var m = sumLoop(0, n);
  assert(m == n);
}
