
// IMPORTS BEGIN
interface Indexable<T> {
	[s: string]: T;
}
// IMPORTS END

/*@ groupBy :: forall T . (array: #Array[#Immutable,T], f: (T)=>string) 
            => { #Indexable[ #Array[#Mutable,T] ] | true } */
function groupBy<T>(array: T[], f: (v: T) => string): any {
  /*@ result :: #Indexable[ #Array[#Mutable,T] ] */
	var result: Indexable<T[]> = {};

  for (var i = 0, n = array.length; i < n; i++) {
    var v = array[i];
    var k = f(v);

    var list: T[] = result[k] || [];
    list.push(v);
    result[k] = list;
  }

	return result;
}
