
/*@ sum :: forall T . (array: #Array[#Immutable,T], f: (T) => number) => { number | true } */

function sum<T>(array: T[], f: (v: T) => number): number {
	var result = 0;

	for (var i = 0, n = array.length; i < n; i++) {
		result = result + f(array[i]);
		//result += f(array[i]);
	}

	return result;
}

