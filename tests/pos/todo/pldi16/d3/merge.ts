/// <reference path="include/d3.d.ts" />

/*@ d3_merge :: <T> (map: IArray<IArray<T>>) => {v: IArray<T> | 0 < 1}*/

function d3_merge<T>(arrays: T[][]): T[] {
  let n = arrays.length;
  let i = -1;
  let j = 0;

  //Original code:
  //while (++i < n) j += arrays[i].length;
  i++;
  while (i < n) {
    j += arrays[i].length;
    i++;
  }

  let merged: IArray<T> = new Array<T>(j);

  //while (--n >= 0) {
  n--;
  while (n >= 0) {
    let array:T[] = arrays[n];
    let m = array.length;
    //while (--m >= 0) {
    m--;
    while (m >= 0 && j > 0) {
      j--;
      merged[j] = array[m];
      m--;
    }
    n--;
  }

  return merged;
};
