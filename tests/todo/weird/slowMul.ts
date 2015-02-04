// This is SAFE but takes insanely long to check. 
// Removing the refinement on x speeds it up to normal speeds.
/*@ foo :: ({number | v > 0 && v*v < 1000000}) => void */
function foo(w) {
    var x = new Array<number>(w);    
    for (var i=0; i<w; i++) x[i] += 1;
}
