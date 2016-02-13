
/*@ minIndex :: (arr:IArray<number>) => {number | 0 < 1} */
function minIndex(arr){
  if (arr.length <= 0) return -1;

  var min = 0;
  // Try making the array accesses unsafe, e.g. by changing < to <=
  for (var i = 0; i < arr.length; i++) {
    var cur = arr[i];
    if (cur < arr[min]){
      min = i;
    }
  }
  return min;
} 
