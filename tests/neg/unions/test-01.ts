
/*@ foo :: (x:number) => number + undefined */
declare function foo(x:number): number;

/*@ bar :: (y: number) => number */
export function bar(y:number):number {
    let z = foo(y);
    if (typeof z === "undefined"){
	    let a = new Array(<number>z);
    	return z;
    }
    return 0;
}
