
/*@ foo :: (x:number) => number + undefined */
function foo(x:number) {
    /*@ res :: number + undefined */
    var res = undefined;
    if (x > 0){
	res = x;
    }
    return res;
}

/*@ inc :: (x:number) => {number | true} */
function inc(x:number):number {
    return x + 1;
}

/*@ bar :: (y:number) => {number | true} */
function bar(y:number):number {
    var z = foo(y);
    if (typeof z === "number"){
	var a = new Array(z);
	return inc(z);
    }
    return 0;
}