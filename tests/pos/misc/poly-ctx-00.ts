/*@ id :: forall A. (A) => A */
function id(x:any):any{
  return x;
}

/*@ foo :: /\ (number)  => {number  | 0 < 1} 
           /\ (boolean) => {boolean | 0 < 1} 
           /\ (string)  => {string  | 0 < 1}
 */
function foo(y){
  var z = id(y);
  return z;
}
