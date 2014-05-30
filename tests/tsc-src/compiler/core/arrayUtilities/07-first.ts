
///<reference path='../../../../../include/prelude.ts' />

/*@ firstOrDefault :: forall T . (array: #Array[#Immutable,T], f : (v: T, index: number) => boolean) => { T | true } */
function firstOrDefault<T>(array: T[], f: (v: T, index: number) => boolean): T {
  for (var i = 0, n = array.length; i < n; i++) {
    var value = array[i];
    if (f(value, i)) {
      return value;
    }
  }

  //PV
  throw new Error("Cannot unify null with T.")
  return null;
}
 
/*@ first :: forall T . (array: #Array[#Immutable,T], f: (v: T, index: number) => boolean) => { T | true } */
//function first<T>(array: T[], f /* ? */: (v: T, index: number) => boolean): T {
function first<T>(array: T[], f: (v: T, index: number) => boolean): T {
 	for (var i = 0, n = array.length; i < n; i++) {
 		var value = array[i];
 		if (!f || f(value, i)) {
 			return value;
 		}
 	}
 
 	throw Errors.invalidOperation("");
}
 
