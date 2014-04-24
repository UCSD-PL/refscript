
/*@ sum :: forall T . (array: [T], func: (T) => number) => bumber */

function sum<T>(array: T[], func: (v: T) => number): number {
	var result = 0;

	for (var i = 0, n = array.length; i < n; i++) {
		result += func(array[i]);
	}

	return result;
}

