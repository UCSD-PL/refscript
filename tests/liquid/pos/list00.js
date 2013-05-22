/*@ hop :: (list [{v:int| 0 <= v}]) => void */
function hop(xs){
  if (empty(xs)) {
    return;
  }
  var h = head(xs);
  assert(0 <= h);
  // var t = tail(xs);
  // return hop(t);
}

