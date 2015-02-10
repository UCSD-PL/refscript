
/*@ bar :: forall T. (this: { IArray<T> | (len v) = 4 }) => { number | v = (len this) } */
function bar () {
  return this.length;
}

var a = bar.call([1,2,3,4]);
assert(a === 4);

