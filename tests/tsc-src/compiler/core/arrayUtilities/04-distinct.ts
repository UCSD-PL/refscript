
// Gets unique element array

/*@ distinct :: forall T . (a: [T], func: (T)=>string) => IIndexable<[T]> */

function distinct<T>(array: T[], equalsFn?: (a: T, b: T) => boolean): T[] {
	/*@ result :: [T] */
	var result: T[] = [];

	// TODO: use map when available
	for (var i = 0, n = array.length; i < n; i++) {
		var current = array[i];
		for (var j = 0; j < result.length; j++) {
			if (equalsFn(result[j], current)) {
				break;
			}
		}

		if (j === result.length) {
			result.push(current);
		}
	}

	return result;
}
