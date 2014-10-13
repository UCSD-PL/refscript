/*@ alias IArray<T> = Array<Immutable, T> */

/*  qualif GT0(n: number)        : 0 <= n      */
/*  qualif Len(v: a, n: number)  : n < (len v) */
/*  qualif Len(v: a)             : (len v) > 0 */
/*  qualif Laa(v: a, n: number)  : (len v > 0) => ((0 <= n) && (n < (len v))) */ 

/*@ predicate inbounds(v, a) = ((0 <= v) && (v < (len a))) */ 


/*  reduce :: forall T A . (IArray<T>, callback: (x: A, y: T, n: number) => A, init:A) => A */

/*@ reduce :: ( me      : { IArray<number> | (len v) > 0 }
              , callback: (x: { number | inbounds(v,me)}, y: number, n: { number | inbounds(v,me)} ) => { number | inbounds(v,me)}
              , init    : { number | inbounds(v,me)} ) 
              => { number | inbounds(v,me)} 
 */

/*  reduce :: ( me      : IArray<number>
              , callback: (x: number, y: number, n: number) => number
              , init    : number) 
              => number
 */
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
      return cur < arr[min] ? i : min; 
  }; 

  return reduce(arr, body, 0);
}

