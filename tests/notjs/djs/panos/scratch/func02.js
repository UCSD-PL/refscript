//NOT WORKING
var quest = function(arr) 
/*: [; L;] (Ref(L)) / (L: { Arr(() -> Top ) | (packed v) } > lArrPro) -> Top / sameType */
{
  var func = function() /*: () -> Top */ {};
  var i /*: { Int | (>= v 0) }*/ = 0;
  /*: (&func: () -> Top) -> (&func: () -> Top) */
  for (i = 0; i < arr.length; i += 1) {
      func = arr[i];
  }
};