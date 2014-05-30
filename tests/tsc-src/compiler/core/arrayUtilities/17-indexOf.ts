
/*@ indexOf :: forall T . (array: #Array[#Immutable,T], predicate: (T) => boolean) 
  => { number | (0 <= v && v < (len array)) } */
function indexOf<T>(array: T[], predicate: (v: T) => boolean): number {
	for (var i = 0, n = array.length; i < n; i++) {
		if (predicate(array[i])) {
			return i;
		}
	}
  throw new Error("Not found");
	//return -1;
}

