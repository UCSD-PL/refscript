// See https://gist.github.com/panagosg7/c83ca8b8226113e6f3d3 for the right fq

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

