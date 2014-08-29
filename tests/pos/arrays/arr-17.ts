

// XXX: TODO : get the comments from object literal types


/*@ where :: ( ) => { #Array[#Immutable,number] | (len v) = 5 } */

function where<T>( ): number[] {
	var result = new Array<number>(5);

	return result;
}

