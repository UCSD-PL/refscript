/*@ plus :: (number, number) => number */
/*@ plus :: (string, string) => string */
function plus(x, y) {
    let z = x + y;
    return z;
}

/*@ foo :: (number) => number */
/*@ foo :: (string) => string */

export function foo(a){
    if (typeof(a) === "number")
	return plus(a, 1);

    return plus(a, "cat");
}
