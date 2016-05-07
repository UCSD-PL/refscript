
/*@ qualif HasP (p: Str, x: a): hasProperty x p */
/*@ qualif EnumP (p: Str, x: a): enumProp x p */

/*@ values :: (map: (Immutable) { [k:string]: { number | v >= 0 }}) => MArray<posint> */
export function values(map: {[k:string]: number}): IArray<number> {
    /*@ readonly values :: MArray<posint> */
    let values: MArray<number> = [];
    for (let key in map) {
        let x = map[key];
        values.push(x);
    }
    return values;
}
