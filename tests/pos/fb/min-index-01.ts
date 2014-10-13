/*@ alias IArray<T> = Array<Immutable, T> */

/*@ reduce :: forall T A . (IArray<T>, callback: (x: A, y: T, n: number) => A, init:A) => A */
function reduce<T,A>(me: T[], callback:(x: A, y: T, n: number) => A, init:A):A{
  var res = init;
  for (var i = 0; i < me.length; i++){
    res = callback(res, me[i], i);
  }
  return res;
}

/*@ minIndex :: (IArray<number>) => {v:number | true} */
function minIndex(arr: number[]): number {

  function body(min: number, cur: number, i: number) { 
    if (min < arr.length) {
      return cur < arr[min] ? i : min 
    }
    return min;
    //  if (cur < arr[min])
    //      min = i;
    //  return min;
  }; 
  return reduce(arr, body, 0);
}

