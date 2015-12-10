
/*@ foo :: (x:number) => number + undefined */
function foo(x:number) {
    /*@ res :: number + undefined */
    var res = undefined;
    if (x > 0){
	    res = x;
    }
    return res;
}

/*@ inc :: (x:number) => {number | 0 < 1} */
function inc(x:number):number {
    return x + 1;
}

/*@ bar :: (y:number) => {number | 0 < 1} */
function bar(y:number):number {
    var z = foo(y);
    if (typeof z === "number"){
	    var a = new Array(<number>z);
    	return inc(z);
    }
    return 0;
}
