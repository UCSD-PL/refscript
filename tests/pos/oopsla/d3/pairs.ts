/// <reference path="../../../d3.d.ts" />
/// <reference path="../../../d3.rsc.ts" />

/*@ d3_pairs :: forall T. ({v:IArray<T> | len(v) > 0}) => IArray<{IArray<T> | len(v) = 2}> */
function d3_pairs <T> (array: T[]) {
  
  var i     = 0; 
  var n     = array.length - 1;
  var p0    = array[0];
  var p1    = p0;
  var pairs = new Array(n < 0 ? 0 : n);

  while (i < n) {
      var ip   = i+1;
      p0       = p1;
      p1       = array[ip];
      pairs[i] = [p0, p1];
      i        = ip;
  }
  return pairs;
};
