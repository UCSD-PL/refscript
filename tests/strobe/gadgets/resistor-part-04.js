
/*@ extern bandNumberValues :: { v:  [ { vv:  number | (0 <= vv && vv < 16) }] | ((len v) = 5) } */



//var bandNumberValues =  [1, 0, 2, 10, 15];


/*@ doSwitchToFourBandResistor :: () => void */
function doSwitchToFourBandResistor() {
    bandNumberValues[2] = 10;
    return;
}


