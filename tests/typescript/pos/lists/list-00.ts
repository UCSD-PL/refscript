/*@ hop :: (#List [{v:number| 0 <= v}]?) => void */
function hop(as : number[]) : void {
  if (empty(as)) {
    return;
  }
  var h = head(as);
  assert(0 <= h);
  var t = tail(as);
  return hop(t);
}

