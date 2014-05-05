
//PV:
//changed local variables `min` and `max` to `min_` and `max_`.

/*@ min :: forall T . (array: { #Array[#Immutable,T] | (len v) > 0 } , f: (T)=>number) => number */
function min<T>(array: T[], f: (v: T) => number): number {
	/*Debug.*/assert(array.length > 0);
	var min_ = f(array[0]);

	for (var i = 1; i < array.length; i++) {
    var next = f(array[i]);
	  if (next < min_) {
	    min_ = next;
	  }
	}

	return min_;
}

/*@ max :: forall T . (array: { #Array[#Immutable,T] | (len v) > 0 }, f: (T)=>number) => number */
function max<T>(array: T[], f: (v: T) => number): number {
   /*debug.*/assert(array.length > 0);
  var max_ = f(array[0]);

  for (var i = 1; i < array.length; i++) {
    var next = f(array[i]);
    if (next > max_) {
      max_ = next;
    }
  }

  return max_;
}
