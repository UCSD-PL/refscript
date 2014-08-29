/*@ foo :: ({ #Array[#Immutable,number] | (len v) >= 1 } ) => number */
function foo(a:number []) : number {
  return a[0];
}

/*@ bar :: (a:#Array[#Immutable,number], i:{number | 0 <= i && i+1 < len a}) => number */
function bar(a:number [], i:number) : number {
  return a[i+1];
}