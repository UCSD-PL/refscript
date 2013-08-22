/* listsum :: (list [{v:number | 0 <= v}]) => list[{v:number ] */

/*@ listsum :: (list [number]) => list[number ] */
function listsum(xs){
  //if (empty(xs)) {
  //  return 0;
  //}
  //var h = head(xs);
  var t = tail(xs);
  var s = t;
  //return h + listsum(t);
  //return listsum(t);
  return s;
}

