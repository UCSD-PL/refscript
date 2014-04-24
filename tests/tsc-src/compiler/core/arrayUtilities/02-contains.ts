
/*@ contains :: forall T . (a: [T]) => boolean */
function contains<T>(array: T[], value: T): boolean {
	/*@ i :: number */
  for (var i = 0; i < array.length; i++) {
    if (array[i] === value) {
      return true;
    }
  }

  return false;
}

