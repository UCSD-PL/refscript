/*@ hop :: (list [{v:number| 0 <= v}]) => list [{v:number| 10 < v}]  + null */
function hop(xs){
  return tail(xs);
//  return t;
}

