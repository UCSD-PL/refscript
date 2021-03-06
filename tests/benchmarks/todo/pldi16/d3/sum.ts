/// <reference path="include/d3.d.ts" />


/*@ d3_sum :: (array : IArray<number>) => {number | 0 < 1} */
/*@ d3_sum :: <T> (array : IArray<T>, f: (T, idx[array]) => number) => {number | 0 < 1} */ 
function d3_sum(array: any, f?:any): number {
  let s = 0;
  let n = array.length;
  let i = 0;
  /*@ a :: number + undefined */
  let a;

  if (arguments.length === 1) {
    while (i < n) {
      a = array[i]; 
      if (!isNaN(a)) { 
        s += <number>a;       // PV added the explicit cast
      }
      i++;
    }
  } else {
    while (i < n) { 
      a = f.call(array, array[i], i);
      if (!isNaN(a)) { 
        s += <number>a;       // PV added the explicit cast
      }
      i++;
    }
  }

  return s;
};
