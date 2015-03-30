
/*@ foo :: /\ (() => number) => { number | true } 
           /\ (number)       => { number | true }
 */
function foo(f) {
  if (typeof f === "function") {
    return f();
  }
  else {
    return f + 1;
  }
}
