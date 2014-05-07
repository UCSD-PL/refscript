
/*@ binarySearch :: (array: #Array[#Immutable,number], value: number) => 
    { v: number | (0 <= v && (v < (len array)))} */
function binarySearch(array: number[], value: number): number {
	var low = 0;
	var high = array.length - 1;

	while (low <= high) {
		//var middle = low + ((high - low) >> 1);
		var middle = low + ((high - low) / 2);
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

  //PV 
  throw new Error("Number not found");
	//return ~low;
}
