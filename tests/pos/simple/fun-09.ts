

/*@ bar :: /\ (() => number) => { number | true } 
           /\ (number)       => { number | true }
 */
function bar(f) {
  return (typeof f === "function") ? f() : f;
}

