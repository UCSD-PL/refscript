
/*@ any :: forall T . (array: [T], f: (T) => boolean) => { boolean | true } */

function any<T>(array: T[], f: (v: T) => boolean): boolean {
	for (var i = 0, n = array.length; i < n; i++) {
		if (f(array[i])) {
			return true;
		}
	}

	return false;
}

