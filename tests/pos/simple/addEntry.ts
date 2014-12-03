  
/*@ addEntry :: forall T . (ob: [Mutable] { [x:string]: T }, entry: { 0: string; 1: T })
             => { [Mutable] {[x: string]: T} | true } */

function addEntry(ob, entry) {
  ob[entry[0]] = entry[1];
  return ob;
}

