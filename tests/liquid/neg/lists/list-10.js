/*@ qualif G11(v:number): v > 10 */
/*@ qualif G11(v:number): v > 9  */

/*@ hop :: (list [{v:number| 9 < v}]) => list [{v:number| 10 < v}]  + null */
function hop(xs){
  var t = tail(xs);
  return t;
}

