 
///<reference path='../../../../../include/prelude.ts' />

/*@ last :: forall T . (array: [T]) => { T | true } */
function last<T>(array: T[]): T {
	if (array.length === 0) {
		throw Errors.argumentOutOfRange('array');
	}

	return array[array.length - 1];
}

/*@ lastOrDefault :: forall T . (array: [T], (v:T, index: number) => boolean) => { T | true }  */
function lastOrDefault<T>(array: T[], predicate: (v: T, index: number) => boolean): T {
  for (var i = array.length - 1; i >= 0; i--) {
    var v = array[i];
    if (predicate(v, i)) {
      return v;
    }
  }

  throw new Error("Cannot unify null with T.")
  return null;
}

