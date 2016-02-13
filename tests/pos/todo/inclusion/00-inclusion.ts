
/*@ qualif HasP (p: Str, x: a): hasProperty x p */
/*@ qualif EnuP (p: Str, x: a): enumProp x p */

/*@ hp :: <A>(obj: A, str: { string | hasProperty obj v }) => void */
declare function hp(ob: {}, str: string): void;

export function values<T>(m: { [k: string]: T }): void {
    for (let k in m) {
        hp(m, k);

        let mk = m[k];
        let a = <T>mk;

        let b = <T>(m[k]);
    }
}

// /*@ bracketRef :: <A>(x: IArray<A> , n: idx<x>)  => A */
// declare function bracketRef<A>(x: IArray<A>, n: number): A;
//
// declare let m: { [k: string]: number };
//
// /*@ ks :: { IArray<{ s: string | hasProperty m s }> | len v > 0 } */
// declare let ks: IArray<string>;
//
// /*@ readonly k :: { string | hasProperty m v } */
// let k = bracketRef(ks, 0);
//
// // let n = <T> (m[k]);
