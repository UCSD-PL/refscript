
/*@ indexOf :: (array: [T], predicate (T) => boolean): boolean */
function indexOf<T>(array: T[], predicate: (v: T) => boolean): number {
	for (var i = 0, n = array.length; i < n; i++) {
		if (predicate(array[i])) {
			return i;
		}
	}
	return -1;
}

