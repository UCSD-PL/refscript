/// <reference path="include/d3.d.ts" />


/*@ qualif HasP<A>(x: string, y: A): hasProperty(x, y) */
/*@ qualif EnumP<A>(x: string, y: A): enumProp(x, y) */

/*@ d3_entries :: forall T . ([Immutable]{ [k:string]: T }) => { MArray<{ key: string; value: T } > | 0 < 1} */
function d3_entries<T>(map) {
    var entries = [];
    for (var key in map) {
        entries.push({ key: key, value: <T>(map[key]) });
    }
    return entries;
};

// TODO: d3.entries = d3_entries;
