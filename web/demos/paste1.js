/////////////////////////////////////////////////////////////////
// Paste Demo 1//////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////


/*@ loop :: (list[int], int, int) => int */
function loop(b, min, i){
  if (i < length(b)){
    if (b[i] < b[0])
      min = i;
    return loop(b, min, i+1)
  }
  return min;
}

/*@ minIndex :: (list[int]) => {v:int|true} */ 
function minIndex(a){
  return loop(a,0,0);
}

