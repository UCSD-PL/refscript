/*@ hop :: (list [{v:number| 0 <= v}] + null) => void */
function hop(as){
  if (empty(as)) {
    return;
  }
  var h = head(as);
  //assert(0 <= h);
  // var t = tail(xs);
  // return hop(t);
}

