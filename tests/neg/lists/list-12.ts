/*@ listsum :: <M extends ReadOnly> (List<M,{v:number | 0 <= v}>) => {v:number| 0 <= v} */
function listsum(xs){
  if (empty(xs)) {
    return 0;
  }
  let h = head(xs);
  let t = tail(xs);
  return h + listsum(t);
}

