
/*@ foo :: /\ (() => number) => { number | 0 < 1 } 
           /\ (number)       => { number | 0 < 1 }
 */
function foo(f) {
  if (typeof f === "function") {
    return f + 1;
  }
  else {
    return f();
  }
}
