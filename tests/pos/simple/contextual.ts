///////////////////////////////////////////////////////////////////////////////
//
//    d3.d.ts
//
///////////////////////////////////////////////////////////////////////////////

/////// Aliases

/*@ alias idx[a]    = {v: number | (0 <= v && v < (len a)) }  */

/////// Qualifiers

/*@ qualif ArrLen(v:number, a:a) : v < (len a) */
/*@ qualif ArrLen(v:number) : 0 <= v */
/*@ qualif ArrLen(a:n, b:m) : (len a) = (len b) */

declare module D3 {
  export interface Base {
    /*@ permute : forall T . (array: #Array[#Immutable, T], 
                  indexes: #Array[#Immutable, #idx[array]]) 
              => { #Array[#Immutable, T] | (len v) = (len indexes) } */
    permute: (arr: any[], indexes: any[]) => any[];
  } 
}

/*@ d3 :: #D3.Base[#Mutable] */
declare var d3: D3.Base;


///////////////////////////////////////////////////////////////////////////////
//
//    arrays/permute.ts
//
///////////////////////////////////////////////////////////////////////////////

d3.permute = function<T>(array: T[], indexes:number[]) : T[] {
  var i = indexes.length, permutes = new Array(i);
  while (i > 0) {
    i--;
    permutes[i] = array[indexes[i]];
  }
  i--;
  return permutes;
};
