
var a = 
  (function () 
  /*@ <anonymous> forall A . (this: A) => A */
  {
    return this;

  }).call([1,2,3]);

assert(a === 4);

