
/*@ foo :: /\ (x: number, y: (number) => number) => {number + undefined | true}
           /\ (x: number) => {number + undefined | true} 
 */ 
function foo(x: any, y?: any): any {
  let bobZooo = arguments.length;
  assert (bobZooo > 0);
  return 1;
}
