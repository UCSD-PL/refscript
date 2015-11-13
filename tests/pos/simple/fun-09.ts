

/*@ bar :: /\ (() => number) => { number | 0 < 1 } 
           /\ (number)       => { number | 0 < 1 }
 */
function bar(f) {
  return (typeof f === "function") ? f() : f;
}

