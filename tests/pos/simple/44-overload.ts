
/*@ bar :: (f: () => posint) => posint */
/*@ bar :: (f: number)       => number */
export function bar(f) {
    return (typeof f === "function") ? f() : f;
}
