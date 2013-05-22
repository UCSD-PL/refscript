/*@ sum :: (list [int]) => int */
function sum(xs){
  if (empty(xs)) {
    return 0;
  }
  var h = head(xs);
  var t = tail(xs);
  return h + t;
}

