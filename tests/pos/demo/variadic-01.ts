

/*@ arr :: {IArray<number> | len v = 4} */
var arr = [1,2,3,4];

var a = 
  (function () 
  /*@ <anonymous> forall A . (this: A) => A */
  {
    return this;

  }).call(arr);

assert(a.length === 4);

