
/*@ alias IArray<T> = Array<Immutable, T> */

/*@ minIndex :: (IArray<number>) => {v:number | true} */
function minIndex(arr){
  function body(min: number, cur: number, i: number) { 

    if (min < arr.length) {
      return cur < arr[min] ? i : min 
    }
    return min;

  }; 
  return arr.reduce(body, 0);
}

