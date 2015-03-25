/// <reference path="../../../d3.d.ts" />

// RJ: dramatically simplifying this as rsc 
//     ensures that no 'undefined' or 'null' 
//     or such in the array

/*@ alias NonEmptyIArray<T> = {IArray<T> | 0 < len v} */

/*@ d3_extent_1 :: forall T . (NonEmptyIArray<T>) => #pair[T] */
function d3_extent_1<T>(array: T[]): T[]{
  var i = 0,
      n = array.length,
      b = array[0],
      a = b,
      c = b;

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

/*@ d3_extent_2 :: forall T U . (NonEmptyIArray<T>, f: (x:T, i:number) => U) => #pair[U] */
function d3_extent_2<T, U>(array: T[], f:(T, number) => U): U[] {
  var i = 0,
      n = array.length,
      b = f.call(array, array[0], 0),
      a = b,
      c = b;

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
/*@ <anonymous> /\ forall T U . (NonEmptyIArray<T>, f: (x:T, i:number) => U) => #pair[U] 
                /\ forall T   . (NonEmptyIArray<T>) => #pair[T] 
 */
{
  if (arguments.length === 1) {
      return d3_extent_1(array);
  } else {
      return d3_extent_2(array, f);
  }
};
