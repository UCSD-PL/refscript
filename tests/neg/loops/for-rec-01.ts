
/*@ forloop :: <A>(number, number, (number, A) => A, A) => A */
export function forloop(lo, hi, body, acc) {
    if (lo < hi) {
        let newAcc = body(acc, lo);
        return forloop(lo + 1, hi, body, newAcc);
    }
    return acc;
}
