
/*@  bar :: (f: () => posint) => { number | v < 0 } */
export function bar(f) {
  return (typeof f === "function") ? f : f();
}
