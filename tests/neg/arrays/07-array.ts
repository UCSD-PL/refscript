
/*@ where :: <T>(values: IArray<T>, ffff: (T) => boolean) => UArray<T> */


/*  where :: <T>(values: IArray<T>, ffff: (T) => boolean) => UArray<T> */
export function where<T>(values: T[], ffff: (v: T) => boolean): T[] {
	let result = new Array<T>(5);
	for (let i = 0; i < values.length; i++) {
		if (ffff(values[i])) {
			result.push(values[i]);
		}
	}
	return result;
}
