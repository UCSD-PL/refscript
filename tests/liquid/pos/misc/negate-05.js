/* negate :: ({xxx: number + boolean | true }) => 
    { ww: number + boolean | (ttag(ww) = ttag(xxx)) } */

/*@ negate :: /\ (number)  => {number  | true}  
              /\ (boolean) => {boolean | true}
 */

function negate(x) {

  if (typeof(4) 
       == 
       "number") {
    return 5;
    // return 0 - x;
  } else {
    return true; //!x;
  }
}

/*@ main :: () => void */
function main(){
  var a = negate(10);
  var b = negate(true);
  return;
}
