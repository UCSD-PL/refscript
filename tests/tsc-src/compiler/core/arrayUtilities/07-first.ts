
/*@ firstOrDefault :: forall T . (array: [T], func : (v: T, index: number) => boolean) => T */
function firstOrDefault<T>(array: T[], func: (v: T, index: number) => boolean): T {
	for (var i = 0, n = array.length; i < n; i++) {
		var value = array[i];
		if (func(value, i)) {
			return value;
		}
	}

	return null;
}

/*@ first :: forall T . (array: [T], func: (v: T, index: number) => boolean) => T */
function first<T>(array: T[], func /*?*/: (v: T, index: number) => boolean): T {
	for (var i = 0, n = array.length; i < n; i++) {
		var value = array[i];
		if (!func || func(value, i)) {
			return value;
		}
	}

	//PV
	//throw Errors.invalidOperation();
	return null;
}

