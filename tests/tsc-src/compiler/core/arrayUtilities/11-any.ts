
/*@ any :: forall T . (array: [T], func (T) => boolean) => boolean */

function any<T>(array: T[], func: (v: T) => boolean): boolean {
	for (var i = 0, n = array.length; i < n; i++) {
		if (func(array[i])) {
			return true;
		}
	}

	return false;
}

