
/*@ qualif HasDP<A>(v:string, s:A): hasDirectProperty(v,s) */
/*@ qualif HasP<A>(v:string, s:A): hasProperty(v,s)        */
/*@ qualif EnumP<A>(v:string, s:A): enumProp(v,s)          */

/*@ extend :: ( src:  [Immutable]{[s:string]: top }, dest: [Mutable]{[s:string]: top})
           => { [Mutable] {[s:string]: top } | true }
 */
function extend(src, dest) {
  for (var p in src) {
    dest[p] = src[p];
  }
  return dest;
}

var options = null;

var this_options = extend({
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
