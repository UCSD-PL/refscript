/*@ hop :: <M extends ReadOnly> (LList<M,{v:number| 0 < v}>) => LList<M,{v:number| 10 < v}> */
function hop(xs){
  return xs;
}

