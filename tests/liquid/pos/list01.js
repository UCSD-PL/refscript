/*@ hop :: (list [{v:number| 0 <= v}]) => list [{v:number| 0 <= v}] */
function hop(xs){
  var t = tail(xs);
  return t;
}

