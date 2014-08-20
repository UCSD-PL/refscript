/*@ plus :: /\ (number, number) => number 
            /\ (string, string) => string */
function plus(x, y) {
    
    var z = x + y;
    return z;
}

/*@ foo :: /\ (number) => {v:number | true}
           /\ (string) => string */

function foo(a){
    if (typeof(a) === "number"){
	var z1 = plus(a, 1);
	return z1;
    }

    return plus(a, "cat");
}