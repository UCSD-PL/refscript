/*@ qualif G11(v:number): v > 10 */
/*@ qualif G11(v:number): v > 9  */

/*@ hop :: <M extends ReadOnly> (List<M,{v:number| 9 < v}>) => LList<M,{v:number| 10 < v}> */
function hop(xs){
  let t = tail(xs);
  return t;
}

