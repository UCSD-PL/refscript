
/*@ alias nat       = {number | 0 <= v}    */

/*@ d3_number :: (x:number + undefined) => {v:boolean | Prop(v) => ttag(x) == "number"} */
function d3_number(x:any):any{
  return x !== null && !isNaN(x);
}
 
/*@ d3_mean :: forall T. (array : IArray<T>, f: (T, {#nat | v < len(array)}) => number + undefined) => number + undefined */
function d3_mean(array: any, f?: any): number {

  /*@ s :: number */
  let s = 0,
      n = array.length;

  /*@ local a :: undefined + number */
  let a;

  let i = 0, j = n;

  while (i < n) {     
    a = f.call(array, array[i], i);
    if (d3_number(a)) { 
    s = s + <number>a
    } else { 
      --j;
    }
    i++;
  }
  if (j) {return s / j;}  else {return undefined;}  
}
