/// <reference path="include/d3.d.ts" />
// <reference path="min.ts" />

// ORIG:
// d3.zip = function(...arrs:any[]):any[] {
//   if (!(n = arguments.length)) return [];
//   for (let i = -1, m = d3.min(arguments, d3_zipLength), zips = new Array(m); ++i < m;) {
//     for (let j = -1, n, zip = zips[i] = new Array(n); ++j < n;) {
//       zip[j] = arguments[j][i];
//     }
//   }
//   return zips;
// };


/*@ d3_zipLength :: <T> (d:IArray<T>, i:number) => nat */
function d3_zipLength(d, i) {
  return d.length;
}

d3.zip = function<T>(args) 
/*@ <anonymous> (args:IArray<IArray<number>>) => {IArray<IArray<number>> | 0 < 1} */
{
  let n = args.length;
  if (!n) return [];

  let m = d3.min(args, d3_zipLength);
  let zips = new Array<IArray<T>>(m);
  for (let i = 0; i < m; i++) {
    let zip = new Array<T>(n);
    zips[i] = zip;
    for (let j = 0; j < n; j++) {
      let tmp = args[j];
      assume(i < tmp.length); // NOTE: check relies on d3_min returning a number <= the length of EVERY input row.
      zip[j] = tmp[i];
    }
  }

  return zips;
}
