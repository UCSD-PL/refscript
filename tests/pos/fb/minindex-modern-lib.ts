
/*@ minIndex :: (a: IArray<number>) => {number | true} */
function minIndex(a){
  
  if (a.length <= 0) return -1;
 
  function body(min: number, cur: number, i: number) { 
      return cur < a[min] ? i : min; 
  }; 

  return a.reduce(body, 0);
}

