function reduce(me, callback, init){
  var res = init;
  for (var i = 0; i < me.length; i++){
    res = callback(res, me[i], i);
  }
  return res;
}

/*@ minIndex :: (arr: top) => { top | true } */
function minIndex(arr){
  function body(min, cur, i){ 
    return cur < arr[min] ? i : min 
  }; 
  return arr.reduce(body, 0);
}

