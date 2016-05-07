/*@ qualif G10(v: int): v > 10 */

/*@ hop :: <M extends ReadOnly> (x: List<M, number>) => List<M, number> */
export function hop(x) {
    return { data: 15, next: null };
}
