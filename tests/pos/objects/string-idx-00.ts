
/*@ qualif HasDP(v: Str, s: a): hasDirectProperty(v,s) */
/*@ qualif HasP (v: Str, s: a): hasProperty(v,s)        */
/*@ qualif EnumP(v: Str, s: a): enumProp(v,s)          */

/*@ extend :: ( src:  { [s:string]: top }, dest: { [s:string]: top }) => {[s:string]: top } */
function extend(src, dest) {
    for (let p in src) {
        dest[p] = src[p];
    }
    return dest;
}

let options = null;

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
}, /*options || */{});
