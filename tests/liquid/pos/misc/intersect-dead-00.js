/*@ foo :: /\ (x:number) => {number  | true}  
           /\ (x:string) => {string  | true}
 */
function foo(x){
  return x;
}

/*@ negate :: /\ (x:number)  => {number  | true}  
              /\ (x:boolean) => {boolean | true}
 */
function negate(x) {
  if (typeof(x) == "number") {
    return crash(); // foo(x);
  }
  return x;
}
