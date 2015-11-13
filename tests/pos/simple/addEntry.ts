  

/*@ alias MMap<T> = [Mutable] {[x:string]: T} */


/*@ addEntry :: forall T . (ob: MMap<T>, entry: { f0: string; f1: T }) => { MMap<T> | 0 < 1 } */

function addEntry(ob, entry) {

  ob[entry.f0] = entry.f1;

  return ob;

}

