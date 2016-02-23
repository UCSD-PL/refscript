/// <reference path="include/d3.d.ts" />

/*@ qualif HasP (x: string, y: A): hasProperty(x, y) */
/*@ qualif EnumP(x: string, y: A): enumProp(x, y) */

/*@ d3_values :: <T> ((Immutable){ [k:string]: T }) => { MArray<T> | 0 < 1 } */
function d3_values<T>(map) {
    /*@ values :: MArray<T> */
    let values = [];
    for (let key in map) {
        values.push(map[key]);
    }
    return values;
};

//TODO: d3.values = d3_values
