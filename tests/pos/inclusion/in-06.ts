
/*@ qualif HasP<A>(v:string, s:A): hasProperty(v,s) */
/*@ qualif EnumP<A>(v:string, s:A): enumProp(v,s)    */

/*@  values :: forall T . (mp: [Immutable] { [k: string]: T }, key: string) =>  { void | 0 < 1 } */
function values<T>(mp: { [k: string]: T }, key: string): void {
    if (key in mp) {
        var a = <T>(mp[key]);
    }
};
