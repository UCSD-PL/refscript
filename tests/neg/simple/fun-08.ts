

/*@ bar :: (() => { number | v < 0 }) => { number | v > 0 } */
function bar(f) {
  return (typeof f === "function") ? f() : f;
}

