/// <reference path="../../../d3.d.ts" />
/// <reference path="../../math/number.ts" />

d3.median = function(array0, f?) 
/*@ <anonymous> /\ forall T. (array : IArray<T>, f: (T) => number) => {number + undefined | true}
                /\           (array : IArray<number>)              => {number + undefined | true} 
 */
{
  /*@ array :: IArray<number> */
  var array = [];
  if (arguments.length > 1) array = array0.map(f);
  else array = array0;

  var array1 = array.filter(d3_number);

  return array1.length ? d3.quantile(array1.sort(d3.ascending), 1/2 ) : undefined;
  // ORIG: return array.length ? d3.quantile(array.sort(d3_ascending), .5 ) : undefined;
};
