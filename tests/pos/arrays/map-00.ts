/*@ foo :: (#iArray[number], (x:number) => string) => {#iArray[string] | true} */ 
function foo(arr:any, f:any) {
  return arr.map(f);
}




