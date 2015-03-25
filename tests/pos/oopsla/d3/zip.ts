/// <reference path="../../../d3.d.ts" />
// <reference path="min.ts" />

// ORIG:
// d3.zip = function(...arrs:any[]):any[] {
//   if (!(n = arguments.length)) return [];
//   for (var i = -1, m = d3.min(arguments, d3_zipLength), zips = new Array(m); ++i < m;) {
//     for (var j = -1, n, zip = zips[i] = new Array(n); ++j < n;) {
//       zip[j] = arguments[j][i];
//     }
//   }
//   return zips;
// };


/*@ d3_zipLength :: forall T . (d:IArray<T>, i:number) => #nat */
function d3_zipLength(d, i) {
  return d.length;
}

d3.zip = function<T>(args) 
/*@ <anonymous> (args:IArray<IArray<number>>) => {IArray<IArray<number>> | true} */
{
  var n = args.length;
  if (!n) return [];

  var m = d3.min(args, d3_zipLength);
  var zips = new Array<Array<T>>(m);
  for (var i = 0; i < m; i++) {
    var zip = new Array<T>(n);
    zips[i] = zip;
    for (var j = 0; j < n; j++) {
      var tmp = args[j];
      assume(i < tmp.length); // NOTE: check relies on d3_min returning a number <= the length of EVERY input row.
      zip[j] = tmp[i];
    }
  }

  return zips;
}
