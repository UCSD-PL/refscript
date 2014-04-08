/*@ qualif G11(v:number): v > 10 */
/*@ qualif G11(v:number): v > 9  */

/*@ hop :: (#List[{v:number| 9 < v}]) => #List[{v:number| 10 < v}] ? */
function hop(xs){
  var t = tail(xs);
  return t;
}

