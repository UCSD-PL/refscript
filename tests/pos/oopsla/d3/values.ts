/// <reference path="../../../d3.d.ts" />

/*@ d3_values :: forall T . ([Immutable]{[k:string]:T}) => {MArray<T> | true} */
function d3_values<T>(map) {
  /*@ values :: MArray<T> */
  var values = [];
  for (var key in map) values.push(map[key]);
  return values;
};

//TODO: d3.values = d3_values
