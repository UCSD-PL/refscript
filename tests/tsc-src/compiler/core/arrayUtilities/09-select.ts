
/*@ select :: forall T S . (values: [T], f: (T) => S) => { [S] | true } */

function select<T, S>(values: T[], f: (v: T) => S): S[] {
	var result: S[] = new Array<S>(values.length);

	for (var i = 0; i < values.length; i++) {
		result[i] = f(values[i]);
	}

	return result;
}

