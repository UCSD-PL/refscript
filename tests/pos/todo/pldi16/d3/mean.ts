/// <reference path="include/d3.d.ts" />
/// <reference path="include/number.ts" />
 
/*@ d3_mean :: <T> (array : IArray<T>, f: (T, idx[array]) => number + undefined) => {number + undefined | 0 < 1} */
/*@ d3_mean ::     (array : IArray<number + undefined>) => {number + undefined | 0 < 1} */ 
function d3_mean(array, f?) {
  let s = 0;
  let n = array.length;
  let i = 0;
  let j = n;
  /*@ local a :: number + undefined */
  let a:number;
  if (arguments.length === 1) {
    while (i < n) { 
      a = array[i];
      if (d3_number(a)) s += <number>a; else j = j - 1;//ORIG: --j;
      ++i;
    }
  } else {
    while (i < n) { 
      a = f.call(array, array[i], i);
      if (d3_number(a)) s += <number>a; else j = j - 1;//ORIG: --j;
      ++i;
    }
  }
  return j ? s / j : undefined; 
}

d3.mean = d3_mean;
