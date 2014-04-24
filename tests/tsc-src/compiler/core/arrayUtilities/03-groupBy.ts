
// IMPORTS BEGIN

interface Indexable<T> {
	[s: string]: T;
}

// IMPORTS END


/*@ groupBy :: forall T . (a: [T], func: (T)=>string) => IIndexable<[T]> */
function groupBy<T>(array: T[], func: (v: T) => string): any {
	/*@ result :: IIndexable<[T]> */
	var result: Indexable<T[]> = {};

	for (var i = 0, n = array.length; i < n; i++) {
		var v: any = array[i];
		var k = func(v);

		/*@ list :: [T] */
		var list: T[] = result[k] || [];
		list.push(v);
		result[k] = list;
	}

	return result;
}
