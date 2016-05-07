
/*@  bar :: (f: () => posint) => posint */
export function bar(f) {
    return (typeof f === "function") ? f() : f;
}
