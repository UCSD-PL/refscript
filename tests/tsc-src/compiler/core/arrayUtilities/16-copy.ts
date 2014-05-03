
/*@ copy :: forall T . 
      (sourceArray: [T] ,
       sourceIndex: { number | 0 <= v } , 
       destinationArray: [T], 
       destinationIndex: { number | 0 <= v } , 
       length: { v:number | ((sourceIndex      + v < (len sourceArray)) 
                          && (destinationIndex + v < (len destinationArray)))}) 
    => void */
function copy<T>(sourceArray: T[], sourceIndex: number, destinationArray: T[], destinationIndex: number, length: number): void {
	for (var i = 0; i < length; i++) {
		destinationArray[destinationIndex + i] = sourceArray[sourceIndex + i];
	}
}

