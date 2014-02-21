/*@ index :: (a: [number], i: {number|(0 <= v && v < (len a))}) => number */
function index(a, i) {
    return a[i];
}
