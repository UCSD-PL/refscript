declare function empty (a:any[]):bool;

/*@ qualif G10(v:number): v > 10 */

/*@ hop :: (list [{v:number| 10 < v}] + null) => list [{v:number| 5 < v}] */
function hop(xs) {
  if (empty(xs)) {
  }
  else {
    return xs;
  }
  return { data: 15, next: null };
}

