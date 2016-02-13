
/*@ qualif HasP<A>(v:string, s:A): hasProperty(v,s) */
/*@ qualif EnumP<A>(v:string, s:A): enumProp(v,s)    */

/*@ values :: (map: [Immutable]{[k:string]: { number | v >= 0 }}) => MArray<{ number | v > 0 }> */
function values(map:{[k:string]: number}): number[] {

  var values = [];

  for (var key in map) {
    values.push(map[key]);
  }

  return values;
};
