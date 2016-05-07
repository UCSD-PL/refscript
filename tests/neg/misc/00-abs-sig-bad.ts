
/*@ abs :: ({ x: number }) => { number | v > x } */
function abs(y) {
    let res = 0;
    if (y > 0) {
        res = y;
    } else {
        res = -y;
    };
    assert(res >= 0);
    return res;
}
