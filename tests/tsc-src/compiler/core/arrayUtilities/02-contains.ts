
/*@ contains :: forall T . (a: #Array[#Immutable,T], value: T) => { boolean | true } */
function contains<T>(array: T[], value: T): boolean {
	/* i :: number */
  for (var i = 0; i < array.length; i++) {
    if (array[i] === value) {
      return true;
    }
  }

  return false;
}

