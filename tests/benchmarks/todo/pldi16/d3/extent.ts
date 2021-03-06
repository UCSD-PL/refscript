/// <reference path="include/d3.d.ts" />

// RJ: dramatically simplifying this as rsc 
//     ensures that no 'undefined' or 'null' 
//     or such in the array

/*@ type NonEmptyIArray<T> = {IArray<T> | 0 < len v} */

/*@ d3_extent_1 :: <T> (NonEmptyIArray<T>) => #pair[T] */
function d3_extent_1<T>(array: T[]): T[]{
  let i = 0;
  let n = array.length;
  let b = array[0];
  let a = b;
  let c = b;

  while (i < n) { 
      b = array[i];
      if ( b !== null) {
	  if (a > b) a = b;
	  if (c < b) c = b;
      }
      i++;
  }
  return [a, c];

};

/*@ d3_extent_2 :: <T,U> (NonEmptyIArray<T>, f: (x:T, i:number) => U) => #pair[U] */
function d3_extent_2<T, U>(array: T[], f:(T, number) => U): U[] {
  let i = 0;
  let n = array.length;
  let b = f.call(array, array[0], 0);
  let a = b;
  let c = b;

  while (i < n) { 
      b = f.call(array, array[i], i);
      if ( b !== null) {
	  if (a > b) a = b;
	  if (c < b) c = b;
      }
      i++;
  }
  return [a, c];
};

d3.extent = function(array: any, f?:any):any 
/*@ <anonymous> /\ <T,U> (NonEmptyIArray<T>, f: (x:T, i:number) => U) => #pair[U] 
                /\ <T>   (NonEmptyIArray<T>) => #pair[T] 
 */
{
  if (arguments.length === 1) {
      return d3_extent_1(array);
  } else {
      return d3_extent_2(array, f);
  }
};
