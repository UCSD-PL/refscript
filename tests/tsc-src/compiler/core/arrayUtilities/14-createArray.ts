
/*@ createArray :: forall T . (length: number, defaultValue: T) => { [T] | true } */
function createArray<T>(length: number, defaultValue: any): T[] {
	var result = new Array<T>(length);
	for (var i = 0; i < length; i++) {
		result[i] = defaultValue;
	}

	return result;
}

