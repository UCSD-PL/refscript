/// <reference path="../../../d3.d.ts" />


/*@ d3_sum :: /\ (array : IArray<number>) => {number | true} 
              /\ forall T. (array : IArray<T>, f: (T, idx[array]) => number) => {number | true} */ 
function d3_sum(array: any, f?:any): number {
  var s = 0;
  var n = array.length;
  var i = 0;
  /*@ a :: number + undefined */
  var a;

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
