
// IMPORTS BEGIN

interface Indexable<number> {
	[s: string]: number;
}

// IMPORTS END


/*@ groupBy :: forall number . (array: #Array[#Mutable,number], f: (number)=>string) 
            => { #Indexable[ #Array[#Immutable,number] ] | true } */
function groupBy<number>(array: number[], f: (v: number) => string): any {
  /*@ result :: #Indexable[ #Array[#Immutable,number] ] */
	var result: Indexable<number[]> = {};

  for (var i = 0, n = array.length; i < n; i++) {
    var v = array[i];
    if (v) {
    var k = f(v);

    var list: number[] = result[k] || [];
    list.push(v);
    result[k] = list;
    }
  }

	return result;
}
