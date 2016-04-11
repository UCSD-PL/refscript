/// <reference path="include/d3.d.ts" />

// d3.keys = function(map: {}): string[]

/*@ keys :: (map: (Immutable){ }) => MArray<{string | hasProperty(v, map) && enumProp(v, map)}> */
function keys(map: any): any[] {
    /*@ keys :: MArray<string> */
    let keys: string[] = [];
    for (let key in map) {
        keys.push(key);
    }
    return keys;
};
