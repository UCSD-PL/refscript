/*@ alias ix<a> = {v:number | 0 <= v && v < len a} */ 

/*@ d3_sum :: /\ (a: IArray<number>) => number
              /\ forall T. (a: IArray<T>, f: (T, ix<a>) => number) => number  */ 
       
function d3_sum (array: any, f?:any): number {
    var s:number = 0;
    var n:number = array.length;
    var i:number = 0;

    if (arguments.length === 1) {
      while (i < n) {
          var a1 = array[i]; 
          if (!isNaN(a1)) { 
 	      s += a1;
 	  }
 	  i++;
      }
    } else {
      while (i < n) { 
	  var a2 = f.call(array, array[i], i);
          if (!isNaN(a2)) { 
      	      s += a2;
      	  }
      	  i++;
      }
    }

  return s;
};
