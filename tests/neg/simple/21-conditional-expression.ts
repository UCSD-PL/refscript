
/*@  bar :: (() => posint) => negint */
export function bar(f) {
  return (typeof f === "function") ? f() : f;
}
