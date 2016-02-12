
/*@ d3_sum ::    (a: IArray<number>) => number */
/*  d3_sum :: <T>(a: IArray<T>, f: (T, idx<a>) => number) => number  */
function d3_sum (array: any, f?:any): number {
    let s = 0;
    let n = array.length;
    let i = 0;

    if (arguments.length === 1) {
        while (i < n) {
            let a1 = array[i];
            if (!isNaN(a1)) {
                s += a1;
    	   }
    	  i++;
      }
    } else {
      while (i < n) {
	//   let a2 = f.call(array, array[i], i);
    //       if (!isNaN(a2)) {
    //   	      s += a2;
    //   	  }
    //   	  i++;
      }
    }

  return s;
};
