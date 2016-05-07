

/*@ bar :: (() => number) => { number | v > 0 } */
/*@ bar :: (number)       =>   number           */
export function bar(f) {
  return (typeof f === "function") ? f : f();
}
