
/*@ last :: forall T . (array: [T]) => T */
function last<T>(array: T[]): T {
	if (array.length === 0) {
		//PV 
		//throw Errors.argumentOutOfRange('array');
		return null;
	}

	return array[array.length - 1];
}

/*@ lastOrDefault :: forall T . (array: [T], (v:T, index: number) => boolean) => T */
function lastOrDefault<T>(array: T[], predicate: (v: T, index: number) => boolean): T {
	for (var i = array.length - 1; i >= 0; i--) {
		var v = array[i];
		if (predicate(v, i)) {
			return v;
		}
	}

	return null;
}

