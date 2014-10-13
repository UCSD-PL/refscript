/*@ alias IArray<T> = Array<Immutable, T> */

function reduce<T,A>(me:T[], callback:(A, T, number) => A, init:A):A{
  var res = init;
  for (var i = 0; i < me.length; i++){
    res = callback(res, me[i], i);
  }
  return res;
}

/*@ minIndex :: (IArray<number>) => {v:number | true} */
function minIndex(arr){
  /*@ body :: (number, number, number) => number */ 
  function body(min, cur, i) { 
    return cur < arr[min] ? i : min 
  }; 
  return reduce(arr, body, 0);
}

