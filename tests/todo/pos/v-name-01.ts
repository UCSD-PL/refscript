
//adapted from navier-stokes
/*@ foo :: (a: IArray<number>, 
            b: {s:IArray<number> | (len s) = 5}) => void */
function foo(u, v) { // changing this v to something else makes it not crash
    for (var j = 1; j < 2; j++) {
        var x = u[42];
    }
}
