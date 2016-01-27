
/*@ bar :: (f: () => number) => { number | v > 0 } */
/*@ bar :: (f: number)       =>   number           */
export function bar(f) {
    return (typeof f === "function") ? f() : f;
}
