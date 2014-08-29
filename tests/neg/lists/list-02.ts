/*@ hop :: (#List[{v:number| 0 < v}]) => #List[{v:number| 10 < v}]? */
function hop(xs){
  var t = tail(xs);
  return t;
}

