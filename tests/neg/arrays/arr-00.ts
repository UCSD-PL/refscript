/*@ foo :: (IArray<number>) => number */
function foo(a) {  
  return a[0];
}

foo([]);
foo([1]);
foo([1,2]);
foo([1,2,3]);
foo([1,2,3,4]);
