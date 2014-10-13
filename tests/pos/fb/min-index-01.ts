/*@ alias IArray<T> = Array<Immutable, T> */

/*@ qualif Len(v: a, arr: b)     : v < (len arr) */

/*@ alias index<a> = {v:number | 0 <= v && v < len a} */ 


/*@  reduce :: forall T A . (arr:IArray<T>, callback: (x: A, y: T, index<arr>) => A, init:A) => A */
function reduce<T,A>(me: T[], callback:(x: A, y: T, n: number) => A, init:A): A {
  var res = init;
  for (var i = 0; i < me.length; i++){
    res = callback(res, me[i], i);
  }
  return res;
}

/*@ minIndex :: ({ IArray<number> | (len v) > 0} ) => number */
function minIndex(arr: number[]): number {

  function body(min: number, cur: number, i: number) { 
      assert(0 <= min);
      assert(min < arr.length);
      return cur < arr[min] ? i : min; 
  }; 

  return reduce(arr, body, 0);
}

