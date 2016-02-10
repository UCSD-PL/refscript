/*@ hop :: <M extends ReadOnly> (LList<M,{v:number| 0 < v}>) => List<M,{v:number| 10 < v}> */
function hop(xs){
  if (! empty(xs)) {
    return xs;
  }
  return { data: 15, next: null };
}

