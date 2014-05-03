
/*@ where :: forall T . (values: [T], f: (T) => boolean) => { [T] | true } */

function where<T>(values: T[], f: (v: T) => boolean): T[] {
	var result = new Array<T>(0);

	for (var i = 0; i < values.length; i++) {
		if (f(values[i])) {
			result.push(values[i]);
		}
	}

	return result;
}

