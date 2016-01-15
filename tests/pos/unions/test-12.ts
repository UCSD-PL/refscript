
/*@ foo :: (x:number) => number + undefined */
function foo(x:number) {
    /*@ res :: number + undefined */
    let res = undefined;
    if (x > 0){
	    res = x;
    }
    return res;
}

function inc(x:number) {
    return x + 1;
}

export function bar(y:number):number {
    let z = foo(y);
    if (typeof z === "number"){
	    return inc(z);
    }
    return 0;
}
