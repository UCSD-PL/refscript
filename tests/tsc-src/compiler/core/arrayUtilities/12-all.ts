
/*@ all :: forall T . (array: [T], func: (T) => boolean) => boolean */

function all<T>(array: T[], func: (v: T) => boolean): boolean {
	for (var i = 0, n = array.length; i < n; i++) {
		if (!func(array[i])) {
			return false;
		}
	}

	return true;
}

