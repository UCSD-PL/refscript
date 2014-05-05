
/*@ contains :: forall T . (a: #Array[#ReadOnly,T], value: T) => { boolean | true } */
function contains<T>(array: T[], value: T): boolean {
	/* i :: number */
  for (var i = 0; i < array.length; i++) {
    if (array[i] === value) {
      return true;
    }
  }

  return false;
}

