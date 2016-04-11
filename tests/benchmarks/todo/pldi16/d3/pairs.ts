/// <reference path="include/d3.d.ts" />
/// <reference path="include/d3.rsc.ts" />

/*@ d3_pairs :: <T> ({v:IArray<T> | len(v) > 0}) => IArray<{IArray<T> | len(v) = 2}> */
function d3_pairs <T> (array: T[]) {
  
  let i     = 0; 
  let n     = array.length - 1;
  let p0    = array[0];
  let p1    = p0;
  let pairs : IArray<IArray<T>> = new Array<IArray<T>>(<number>(n < 0 ? 0 : n));

  while (i < n) {
      let ip   = i+1;
      p0       = p1;
      p1       = array[ip];
      pairs[i] = [p0, p1];
      i        = ip;
  }
  return pairs;
};
