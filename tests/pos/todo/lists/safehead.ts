
/*@ getHead :: <M extends ReadOnly>(LList<M, number>) => { number | 0 < 1 } */
function getHead(xs){
  if (empty(xs)) {
    return 1;
  }
  return safehead(xs);
}
