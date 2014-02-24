/*@ listsum :: (list [{v:number | 0 <= v}]) => {v:number| 0 <= v} */
function listsum(xs){
  if (empty(xs)) {
    return 0;
  }
  var h = head(xs);
  var t = tail(xs);
  return h + listsum(t);
}

