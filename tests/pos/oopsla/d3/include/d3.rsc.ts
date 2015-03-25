/// <reference path="d3.d.ts" />

/*@ bv_to_num :: (bv: bitvector32) => { number | true } */
declare function bv_to_num(bv: number): number;

/*@ num_to_bv :: (n: number) => { bitvector32 | true } */
declare function num_to_bv(n: number): number;

/*@ check_undefined :: forall T.(T + undefined) => {T | true} */
function check_undefined<T>(x:any) : T{
    if (typeof x === "undefined") 
	      return crash();
    return <T>x;
}

/*@ randomN :: (n : #nat) => {v : #nat | v < n} */
function randomN(n:number):number {
    var r = Math.random() * n;
    // r = bv_to_num(num_to_bv(r) | num_to_bv(0));
    assume (0 <= r && r < n);
    return r;
}
