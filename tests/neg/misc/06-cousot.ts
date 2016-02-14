// DEMO EXAMPLE. Crucially requires:

/*@ qualif Equal(v:number, x:number) : v = x */

// Note that the example works even "without" the above,
// as this qualifier is in include/prelude.js

/*@ loop :: (number, number) => void */

function loop(n, m) {

    if (n === 0) {
        assert(m === 0);
        return;
    }

    if (random() > 0) {
        n = n + 1;
        m = m + 1;
    }

    if (random() > 0) {
        n = n - 1;
        // m = m - 1;
    }

    loop(n, m);
}

/*@ main :: ({n:number| n > 0}) => void */
function main(n) {
    loop(n, n);
}
