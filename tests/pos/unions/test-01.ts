
/*@ negate :: (x: { number | v > 0 } + boolean) => { number | v < 0 } + boolean */
function negate(x): any {
    // return (typeof (x) === "number") ? (0 - x) : (!x);

    if (typeof (x) === "number") {
        return (0 - x);
    }
    else {
        return (!x);
    }
}
