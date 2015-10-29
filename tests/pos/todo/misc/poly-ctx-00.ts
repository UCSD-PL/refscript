/*@ id :: forall A. (A) => A */
function id(x:any):any{
  return x;
}

/*@ foo :: /\ (number)  => {number  | true} 
           /\ (boolean) => {boolean | true} 
           /\ (string)  => {string  | true}
 */
function foo(y){
  let z = id(y);
  return z;
}
