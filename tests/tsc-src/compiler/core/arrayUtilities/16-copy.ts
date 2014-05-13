
/*@ copy :: forall T . 
      (sourceArray: #Array[#Immutable,T] ,
       sourceIndex: { number | 0 <= v } , 
       destinationArray: #Array[#Immutable,T], 
       destinationIndex: { number | 0 <= v } , 
       length: { v:number | ((sourceIndex      + v <= (len sourceArray)) 
                          && (destinationIndex + v <= (len destinationArray)))}) 
    => void */
function copy<T>(sourceArray: T[], sourceIndex: number, destinationArray: T[], destinationIndex: number, length: number): void {
	for (var i = 0; i < length; i++) {
		destinationArray[destinationIndex + i] = sourceArray[sourceIndex + i];
	}
}

