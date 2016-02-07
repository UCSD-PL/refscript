
/*@ where :: <T>(values: IArray<T>, f: (T) => boolean) => UArray<T> */
export function where<T>(values: T[], f: (v: T) => boolean): T[] {
	let global: IArray<T> = [];

	let result = new Array<T>(5);
	for (let i = 0; i < values.length; i++) {
		if (f(values[i])) {
			result.push(values[i]);
		}
	}
	return result;
}
