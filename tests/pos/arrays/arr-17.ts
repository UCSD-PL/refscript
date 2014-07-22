
/*@ where :: ( ) => { #Array[#Immutable,number] | (len v) = 5 } */

function where<T>( ): number[] {
	var result = new Array(5);

	return result;
}

