
/*@ foo :: /\ (x: number, y: (number) => number) => {number + undefined | 0 < 1 }
           /\ (x: number) => {number + undefined | 0 < 1 } 
 */ 
function foo(x: any, y?: any): any {
  var bobZooo = arguments.length;
  assert (bobZooo <= 1);
  return 1;
}
