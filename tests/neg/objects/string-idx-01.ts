
/*@ qualif HasP (v: Str, s:a): hasProperty(v,s) */
/*@ qualif EnumP(v: Str, s:a): enumProp(v,s) */

/*@ extend :: (src: (Immutable) { [s:string]: top }, dest: (Mutable) { [s:string]:top }) => (Mutable) { [s:string]:top } */
export function extend(src, dest) {
    for (let p in src) {
        dest[p] = src[p];
    }
    return dest;
}

/*@ readonly options :: { [s: string]: top } */
let options = {};

let this_options = extend({
    canvasHeight: 100,
    canvasWidth: 100,
    pixelWidth: 2,
    pixelHeight: 2,
    renderDiffuse: false,
    renderShadows: false,
    renderHighlights: false,
    renderReflections: false,
    rayDepth: 2
}, options /*|| {}*/);
