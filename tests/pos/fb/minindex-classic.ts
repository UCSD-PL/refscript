
/*@ minIndex :: (arr:IArray<number>) => {number | true} */
function minIndex(arr){
  if (arr.length <= 0) return -1;

  var min = 0;
  for (var i = 0; i < arr.length; i++) {
    var cur = arr[i];
    if (cur < arr[min]){
      min = i;
    }
  }
  return min;
} 
