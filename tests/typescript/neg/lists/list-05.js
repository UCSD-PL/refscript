/*@ hop :: (list [{v:number| 0 < v}] + null) => list [{v:number| 10 < v}] + null */
function hop(xs){
  if (! empty(xs)) {
    return xs;
  }
  return { data: 15, next: null };
}

