// neg version of d3/arrays/permute.ts

/*@ alias idx[a]    = {v: number | (0 <= v && v < (len a)) }  */

/*@ qualif ArrLen(v:number, a:a) : v < (len a) */
/*@ qualif ArrLen(a:n, b:m) : (len a) = (len b) */

/*@ d3_permute :: forall T . (array: IArray<T>, 
                              indexes: IArray<#idx[array]>) 
                          => { IArray<T> | (len v) = (len indexes) } */
function d3_permute<T>(array: T[], indexes:number[]) : T[] {
  var i = indexes.length, permutes: T[] = new Array<T>(i);
  // while (i--) permutes[i] = array[indexes[i]];
  while (i) {
    i--;
    permutes[i] = array[indexes[i+1]];
  }
  i--;
  return permutes;
};
