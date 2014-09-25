

/*@ bar :: /\ (() => number) => { number | v > 0 } 
           /\ (number)       => { number | true }
 */
function bar(f) {
  return (typeof f === "function") ? f() : f;
}

