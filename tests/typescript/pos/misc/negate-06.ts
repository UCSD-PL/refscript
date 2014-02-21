
/*@ negate :: /\ (x:number)  => {number  | v > x}  
              /\ (x:boolean) => {boolean | true}
 */



function negate(x) {
  if (typeof(x) == "number") {
    return x + 1;
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
