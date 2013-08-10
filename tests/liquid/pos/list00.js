// See https://gist.github.com/panagosg7/c83ca8b8226113e6f3d3 for the right fq

/*@ hop :: (list [{v:number| 0 <= v}]) => void */
function hop(xs){
  if (empty(xs)) {
    return;
  }
  var h = head(xs);
  assert(0 <= h);
  // var t = tail(xs);
  // return hop(t);
}

