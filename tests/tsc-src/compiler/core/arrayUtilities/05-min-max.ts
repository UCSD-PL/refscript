
/*@ min :: forall T . (array: [T], func: (T)=>number) => number */
function min<T>(array: T[], func: (v: T) => number): number {
	// Debug.assert(array.length > 0);
	var min = func(array[0]);

	for (var i = 1; i < array.length; i++) {
		var next = func(array[i]);
		if (next < min) {
			min = next;
		}
	}

	return min;
}

/*@ max :: forall T . (array: [T], func: (T)=>number) => number */
function max<T>(array: T[], func: (v: T) => number): number {
	// Debug.assert(array.length > 0);
	var max = func(array[0]);

	for (var i = 1; i < array.length; i++) {
		var next = func(array[i]);
		if (next > max) {
			max = next;
		}
	}

	return max;
}
