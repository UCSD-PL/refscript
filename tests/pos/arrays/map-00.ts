/*@ foo :: (#Array[#Immutable, number], (x:number) => string) => {#Array[#Immutable, string] | 0 < 1} */ 
function foo(arr:any, f:any) {
  return arr.map(f);
}




