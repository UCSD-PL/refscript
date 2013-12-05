/* negate :: ({xxx: number + boolean | true }) => 
    { ww: number + boolean | (ttag(ww) = ttag(xxx)) } */

/*@ negate :: /\ (number)  => {number  | true}  
              /\ (boolean) => {boolean | true}
 */

function negate(x) {
  if (typeof(x) == "number") {
    return 0 - x;
  } else {
    return !x;
  }
}

/*@ main :: () => void */
function main(){
  var a = negate(10);
  var b = negate(true);
  return;
}
