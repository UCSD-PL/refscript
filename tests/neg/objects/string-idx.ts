
/*@ qualif HasP(v:string, s:a): hasProperty(v,s) */
/*@ qualif EnumP(v:string, s:a): enumProp(v,s) */

/*@ extend :: (
    src:[Immutable]{[s:string]:string}
  , dest:[Mutable]{[s:string]:top}
  ) => {[Mutable]{[s:string]:top} | true} */
function extend(src, dest) {
  for (var p in src) {
    dest[p] = src[p];
  }
  return dest;
}

var options = {};

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
  }, options || {});
