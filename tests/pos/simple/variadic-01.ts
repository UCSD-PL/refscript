
/*@ qualif Length(v:number): (len v) = 4 */

var a = 
  (function () 
  /*@ <anonymous> forall A . (this: A) => A */
  {
    return this;

  }).call([1,2,3,4]);

assert(a.length === 4);

