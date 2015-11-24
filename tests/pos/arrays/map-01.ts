
/*@ bar :: (IArray<number>, (number, number) => string) => { IArray<string> | 0 < 1} */ 
function bar(arr, f) {
  return arr.map(f);
}
