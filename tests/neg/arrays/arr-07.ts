
/*@ where :: forall T . (values: IArray<T>, f: (T) => boolean) 
          => { IArray<T> | (len v) = 5 } */

function where<T>(values: T[], f: (v: T) => boolean): T[] {
	let result = new Array<T>(5);

	for (let i = 0; i < values.length; i++) {
		if (f(values[i])) {
			result.push(values[i]);
		}
	}

	return result;
}