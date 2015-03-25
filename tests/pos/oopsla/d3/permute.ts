/// <reference path="../../../d3.d.ts" />

//d3.permute = function<T>(array: T[], indexes:number[]) : T[] {

/*@ d3_permute :: forall T . (array: IArray<T>, 
                              indexes: IArray<#idx[array]>) 
                          => { IArray<T> | (len v) = (len indexes) } */
function d3_permute<T>(array: T[], indexes:number[]) : T[] {
  var i = indexes.length, permutes: T[] = new Array<T>(i);
  // while (i--) permutes[i] = array[indexes[i]];
  while (i) {
    i--;
    permutes[i] = array[indexes[i]];
  }
  i--;
  return permutes;
};
