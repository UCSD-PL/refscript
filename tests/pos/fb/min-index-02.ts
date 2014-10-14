/*@ alias IArray<T> = Array<Immutable, T> */

/*@ alias NEArray<T> = {Array<Immutable, T> | 0 < len v}  */

/*@ qualif Len(v: a, n: number)  : n < (len v) */


/*@ minIndex :: (NEArray<number>) => number */
function minIndex(arr){
  
  function body(min: number, cur: number, i: number) { 
    return cur < arr[min] ? i : min 
  }; 

  return arr.reduce(body, 0);
}

