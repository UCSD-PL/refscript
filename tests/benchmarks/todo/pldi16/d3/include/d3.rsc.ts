/// <reference path="d3.d.ts" />

/*@ bv_to_num :: (bv: bitvector32) => { number | 0 < 1 } */
declare function bv_to_num(bv: number): number;

/*@ num_to_bv :: (n: number) => { bitvector32 | 0 < 1 } */
declare function num_to_bv(n: number): number;

/*@ check_undefined :: <T> (T + undefined) => {T | 0 < 1} */
function check_undefined<T>(x:any) : T{
    if (typeof x === "undefined") 
	      return crash();
    return <T>x;
}

/*@ randomN :: (n : #nat) => {v : #nat | v < n} */
function randomN(n:number):number {
    let r = Math.random() * n;
    // r = bv_to_num(num_to_bv(r) | num_to_bv(0));
    assume (0 <= r && r < n);
    return r;
}
