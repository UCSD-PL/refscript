
/*@ qualif Add(v: number, n: number, m: number): v = m + n */

/*@ lin_solve :: () => void */
function lin_solve() {
    var x = 6;
    // ++x;
    for (var i = 1; i <= 1; i++) {
        ++x;
        assert(x < 9);
    }
}
