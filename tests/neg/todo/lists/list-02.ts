/*@ hop :: <M extends ReadOnly> (List<M,{v:number| 0 < v}>) => LList<M,{v:number| 10 < v}> */
function hop(xs){
  let t = tail(xs);
  return t;
}

