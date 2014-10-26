
/*@ bar :: (IArray<number>, (number, number) => string) => { IArray<string> | true} */ 
function bar(arr, f) {
  return arr.map(f);
}



