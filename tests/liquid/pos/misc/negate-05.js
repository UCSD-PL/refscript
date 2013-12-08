/*@ negate :: /\ (number)  => {number  | true}  
              /\ (boolean) => {boolean | true}
 */

function negate(x) {
  if (typeof(x) == "number") {
    return 5;
    // return 0 - x;
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
