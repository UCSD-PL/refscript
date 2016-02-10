/*@ hop :: <M extends ReadOnly> (LList<M,{v: number | v >= 0}>) => void */
function hop<M extends ReadOnly>(as? : List<M,number>) : void {
  if (empty(as)) {
    return;
  }
  let h = head(as);
  assert(0 <= h);
  let t = tail(as);
  return hop(t);
}
