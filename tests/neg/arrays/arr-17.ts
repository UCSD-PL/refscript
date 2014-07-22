
/*@ where :: ( ) => { #Array[#Immutable,number] | (len v) = 5 } */

function where<T>( ): number[] {
	var result = new Array(20);

	return result;
}

