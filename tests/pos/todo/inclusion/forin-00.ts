
/*@ qualif HasP<A>(x: string, y: A): hasProperty(x, y) */
/*@ qualif EnumP<A>(x: string, y: A): enumProp(x, y) */


/*@  values :: forall T . (mp: [Immutable]{ [k:string]: T }) =>  { void | 0 < 1 } */
function values<T>(mp: { [k: string]: T }): void {

    for (var key in mp) {
        var a = <T>mp[key];
    }

};
