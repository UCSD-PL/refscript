
/*@ alias IArray<T> = Array<Immutable, T> */


/*@ qualif Len(v: a, n: number)  : n < (len v) */

/*@ minIndex :: (IArray<number>) => {v:number | true} */
function minIndex(arr){
  
  function body(min: number, cur: number, i: number) { 
    return cur < arr[min] ? i : min 
  }; 

  return arr.reduce(body, 0);
}

