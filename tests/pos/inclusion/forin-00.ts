/*@ option --extrainvs */

/*@ qualif HasP<A>(v:string, s:A): hasProperty(v,s) */
/*@ qualif EnumP<A>(v:string, s:A): enumProp(v,s)    */

/*@  values :: forall T . (mp: [Immutable]{ [k:string]: T }) =>  { MArray<T> | true } */
function values<T>(mp:{[k:string]:T}): T[] {

  var values:T[] = [];

  for (var key in mp) {
    values.push(<T>(mp[key]));
  }

  return values;
};
