/*@ plus :: /\ (number, number) => number 
            /\ (string, string) => string */
function plus(x, y) {
    
    var z = x + y;
    return z;
}

/*@ foo :: /\ (number) => number
           /\ (string) => string */

function foo(a){
    if (typeof(a) === "number")
	return plus(a, 1);

    return plus(a, "cat");
}
