
/*@ qualif Bot(v:a,s:string): keyIn(v,s) */

/*@  values :: forall T . (map: [#Immutable]{ [k:string]: T }) =>  { #Array[#Mutable, T] | true } */
function values<T>(map:{[k:string]:T}): T[] {
  
  var values:T[] = [];
  
  for (var key in map) { 
    values.push(map[key]);
  }
  
  return values;
};
