
/*@ foo :: forall T. (T, {v:number | v > 0}) => T + undefined */
function foo<T>(x:T, n:number) {
  var i = n;

  /*@ a :: T + undefined */
  var a = undefined;

  while (0 < i){
    if (i > 100) {
      a = x;
    } else {
      a = undefined;
    }
    i--;
  }

  return a;
}
