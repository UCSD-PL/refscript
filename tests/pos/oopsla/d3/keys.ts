/// <reference path="include/d3.d.ts" />

// d3.keys = function(map: {}): string[]

/*@ keys :: (map: [Immutable]{ }) => MArray<{string | hasProperty(v, map) && enumProp(v, map)}> */
function keys(map: any): any[] {
    var keys: string[] = [];
    for (var key in map) {
        keys.push(key);
    }
    return keys;
};
