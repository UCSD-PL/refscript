
/*@ foo :: /\ (x: number) => number 
           /\ (x: string) => string */
function foo(x: any): any {
  return x;
}

var a = foo(1);


