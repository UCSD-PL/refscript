/*@ qualif UBound<A>(v:number, x:A) : v < (len x) */

/*@ loop :: (#Array[#Immutable,number], number, number) => number */
function loop(b: number[], acc: number, i: number): number {
  if (i < b.length) {
    var acc_ = acc;
    assert(i < b.length);
    if (b[i] < b[acc]) {
      acc_ = i;
  	}
    return loop(b, acc_, i + 1)
  }
  return acc;
}

/*@ minIndex :: ({a: #Array[#Immutable,number] | 0 < (len a)}) => {v:number | (0 <= v && v < (len a))} */
function minIndex(a){
  var r = loop(a, 0, 0);
  return r;
}
