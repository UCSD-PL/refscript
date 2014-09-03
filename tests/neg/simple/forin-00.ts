
/*@ values :: forall T . (map: {[k:string]: { number | v >= 0 }}) => { number | v > 0 } */
function values<T>(map:{[k:string]:T}):T[] {
  var values:T[] = [];
  
  for (var key in map) values.push(map[key]);
  
  return values;
}
