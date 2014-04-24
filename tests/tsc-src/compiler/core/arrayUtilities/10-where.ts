
/*@ where :: forall T . (values: [T], func: (T) => boolean) => [T] */

function where<T>(values: T[], func: (v: T) => boolean): T[] {
	var result = new Array<T>();

	for (var i = 0; i < values.length; i++) {
		if (func(values[i])) {
			result.push(values[i]);
		}
	}

	return result;
}

