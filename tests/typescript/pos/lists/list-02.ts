/*@ qualif G10(v:number)   : v > 10    */

/*@ hop :: (#List [{v:number| 10 < v}]?) => #List [{v:number| 5 < v}] */
function hop(xs){
  if (empty(xs)) {
  }
  else {
    return xs;
  }
  return { data: 15, next: null };
}

