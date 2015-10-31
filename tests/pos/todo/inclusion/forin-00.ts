
/*@ qualif HasP(v:string, s:A): hasProperty(v,s) */
/*@ qualif EnumP(v:string, s:A): enumProp(v,s)    */

/*@  values :: forall T . (map: [Immutable]{ [k:string]: T }) =>  { MArray<T> | true } */
function values<T>(map:{[k:string]:T}): T[] {
  
  var values:T[] = [];
  
  for (var key in map) { 
    values.push(<T>(map[key]));
  }
  
  return values;
};
