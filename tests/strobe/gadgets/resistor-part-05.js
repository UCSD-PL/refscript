
/*@ numberOfColorBands :: { number | ( 4 <= v && v <= 5)} */
var numberOfColorBands;
 
/*@ toggleNumberOfBandsButton :: () => void */
function toggleNumberOfBandsButton() {
    if (numberOfColorBands == 4) {
        numberOfColorBands = 5;
    } else { // numberOfColorBands == 5.
        numberOfColorBands = 4;
    }


    return;
}

