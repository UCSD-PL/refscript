/*@ qualif G10(v:number)   : v > 10    */

/*@ hop :: <M extends ReadOnly> (List<M,{v:number| 10 < v}>) => List<M,{v:number| 5 < v}> */
/*@ hop :: (null) => List<Unique,{v:number| 5 < v}> */
function hop(xs){
  if (empty(xs)) {
  }
  else {
    return xs;
  }
  return { data: 15, next: null };
}

