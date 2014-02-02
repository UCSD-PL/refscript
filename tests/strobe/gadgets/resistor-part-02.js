
/*@ removeCommas :: (inputStr: string) => { string | true } */
function removeCommas(inputStr) {
    var noCommasValue = "";
    var index = 0;

    for (var i = 0; i < inputStr.length; i++) {
        if (inputStr.charAt(i) != ",") {
            noCommasValue =  1 ;// inputStr.charAt(i);
        }
    }

    return (noCommasValue);
}

