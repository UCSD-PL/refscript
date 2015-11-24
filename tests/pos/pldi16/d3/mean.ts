/// <reference path="include/d3.d.ts" />
/// <reference path="include/number.ts" />
 
/*@ d3_mean :: 
    /\ forall T. (array : IArray<T>, f: (T, idx[array]) => number + undefined) => {number + undefined | 0 < 1}
    /\           (array : IArray<number + undefined>) => {number + undefined | 0 < 1} */ 
function d3_mean(array, f?) {
  var s = 0,
      n = array.length,
      i = 0,
      j = n;
  /*@ local a :: number + undefined */
  var a:number;
  if (arguments.length === 1) {
    while (i < n) { 
      a = array[i];
      if (d3_number(a)) s += <number>a; else --j;
      ++i;
    }
  } else {
    while (i < n) { 
      a = f.call(array, array[i], i);
      if (d3_number(a)) s += <number>a; else --j;
      ++i;
    }
  }
  return j ? s / j : undefined; 
}

d3.mean = d3_mean;
