
/*@ type MMap<T> = (Mutable) {[x:string]: T} */
type MMap<T> = { [x: string]: T };

/*@ addEntry :: <T>(ob: MMap<T>, entry: { f0: string; f1: T }) => MMap<T> */
export function addEntry(ob, entry) {
    ob[entry.f0] = entry.f1;
    return ob;
}
