
/*@ id :: forall A. (A) => A */
function id(x){
  return x;
}

/*@ foo :: /\ (number)  => {number  | true} 
           /\ (boolean) => {boolean | true} 
           /\ (string)  => {string  | true}
 */

/* foo :: (number)  => {number  | true} */
function foo(y){
  var z = id(y);
  return z;
}
