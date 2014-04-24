
/*@ copy :: (sourceArray: [T] , sourceIndex: number, destinationArray: [T], destinationIndex: number, length: number) => void */
function copy<T>(sourceArray: T[], sourceIndex: number, destinationArray: T[], destinationIndex: number, length: number): void {
	for (var i = 0; i < length; i++) {
		destinationArray[destinationIndex + i] = sourceArray[sourceIndex + i];
	}
}

