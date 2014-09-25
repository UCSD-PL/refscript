

/*@ foo :: /\ (() => number) => { number | true } 
           /\ (number)       => { number | true }
 */
function foo(f) {
  if (typeof f === "function") {
    return f + 1;
  }
  else {
    return f();
  }
}
