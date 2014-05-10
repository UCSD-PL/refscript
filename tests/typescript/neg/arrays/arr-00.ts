
/*@ foo :: (#Array[#Immutable,number]) => number */
function foo(a) {  
  return a[0];
}

foo([]);
