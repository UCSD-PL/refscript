
/*@ bar :: (arr: IArray<number>, fn: (number, number) => string) => { IArray<string> | true} */ 
function bar(arr, fn) {
  return arr.map(fn);
}



