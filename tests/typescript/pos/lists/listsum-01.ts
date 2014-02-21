/*@ sum :: (list [{v:number| 0 <= v}]) => number */
function sum(xs){
  if (empty(xs)) {
    return 0;
  }
  var h = head(xs);
  var t = tail(xs);
  var z = sum(xs);

  return 10; // h + sum(t);
}

