
/*@ alias IArray<T> = {Array<Immutable, T> | true} */


/*@ minIndex :: (IArray<number>) => number */
function minIndex(arr){
  var min = 0;
  for (var i = 0; i < arr.length; i++) {
    var cur = arr[i];
    if (cur < arr[min]){
      min = i;
    }
  }
  return min;
} 
