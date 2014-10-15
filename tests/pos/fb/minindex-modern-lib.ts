/*@ minIndex :: (arr:IArray<number>) => {number | true} */
function minIndex(arr){
  
  if (arr.length <= 0) return -1;
 
  function body(min: number, cur: number, i: number) { 
      return cur < arr[min] ? i : min 
  }; 

  return arr.reduce(body, 0);
}

