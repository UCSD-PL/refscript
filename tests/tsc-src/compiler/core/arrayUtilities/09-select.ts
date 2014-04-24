
/*@ select :: forall T S . (values: [T], func: (T)=>S) => [S] */

function select<T, S>(values: T[], func: (v: T) => S): S[] {
	/*@ result :: [S] */
	var result: S[] = new Array<S>(values.length);

	for (var i = 0; i < values.length; i++) {
		result[i] = func(values[i]);
	}

	return result;
}

