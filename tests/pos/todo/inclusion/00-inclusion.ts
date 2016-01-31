
/*@ qualif HasP (x: a, p: Str): hasProperty x p */
/*  qualif EnumP(x: a, p: Str): enumProp x p    */

// export function values<T>(m: { [k: string]: T }): void {
//     for (let k in m) {
//         let a = <T>m[k];
//     }
// }

/*@ bracketRef :: <A>(x: IArray<A> , n: idx<x>)  => A */
declare function bracketRef<A>(x: IArray<A>, n: number): A;

declare let m: { [k: string]: number };

/*@ ks :: { IArray<{ s: string | hasProperty m s }> | len v > 0 } */
declare let ks: IArray<string>;

/*@ readonly k :: { string | hasProperty m v } */
let k = bracketRef(ks, 0);


// let n = <T> (m[k]);
