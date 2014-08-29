
/*@ where :: ( ) => { #Array[#Immutable,number] | (len v) = 5 } */

function where<T>( ): number[] {
	var result = new Array(5);

	while (result.push(1)) {
  
  }

	return result;
}

