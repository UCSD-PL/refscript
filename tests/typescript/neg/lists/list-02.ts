/*@ hop :: (list [{v:number| 0 < v}]) => list [{v:number| 10 < v}]  + null */
function hop(xs){
  var t = tail(xs);
  return t;
}

