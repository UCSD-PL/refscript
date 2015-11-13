

/*@ bar :: /\ (() => number) => { number | v > 0 } 
           /\ (number)       => { number | 0 < 1 }
 */
function bar(f) {
  return (typeof f === "function") ? f() : f;
}

