
/*@ listsum :: (list [number]) => number */
function listsum(xs) {
  var t = tail(xs);
  return listsum(t);
}

