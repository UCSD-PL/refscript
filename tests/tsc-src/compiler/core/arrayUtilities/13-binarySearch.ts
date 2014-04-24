

/*@ binarySearch :: (array: [number], value: number): number */

function binarySearch(array: number[], value: number): number {
	var low = 0;
	var high = array.length - 1;

	while (low <= high) {
		var middle = low + ((high - low) >> 1);
		var midValue = array[middle];

		if (midValue === value) {
			return middle;
		}
		else if (midValue > value) {
			high = middle - 1;
		}
		else {
			low = middle + 1;
		}
	}

	return ~low;
}

