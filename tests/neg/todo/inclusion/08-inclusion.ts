
/*@ qualif HasP (p: Str, x: a): hasProperty x p */
/*@ qualif EnumP (p: Str, x: a): enumProp x p */

/*@ values :: (map: (Immutable) { [k:string]: { number | v >= 0 }}) => MArray<{ number | v > 0 }> */
function values(map: {[k:string]: number}): IArray<number> {

    /*@ local value :: MArray<posint> */
    let values: MArray<number> = [];

    for (let key in map) {
      values.push(<number>map[key]);
    }
    return values;
};
