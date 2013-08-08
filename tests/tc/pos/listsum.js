/*@ sum :: (list [number]+null) => number */
function sum(xs){
  if (empty(xs)) {
    return 0;
  }
  var h = head(xs);
  var t = tail(xs);
  return h + sum(t);
}

