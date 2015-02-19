/*@ alias index<a> = {v:number | 0 <= v && v < len a} */ 


// function reduce<T,A>(me: T[], callback:(x: A, y: T, n: number) => A, init:A): A


/*@ reduce :: forall T A . (arr:IArray<T>, callback: (x: A, y: T, index<arr>) => A, init:A)
           => A */
function reduce(me, callback, init) {
  var res = init;

  for (var i = 0; i < me.length; i++){
    res = callback(res, me[i], i);
  }
  
  return res;
}


/*@ minIndex :: (arrrr:IArray<number>) => {number | true} */
function minIndex(arrrr) {

    var arr /*@ readonly */ = arrrr;

    if (arr.length <= 0) return -1;
    
    function body(min: number, cur: number, i: number) { 
	    return cur < arr[min] ? i : min; 
    }; 
    
    return reduce(arr, body, 0);
}























