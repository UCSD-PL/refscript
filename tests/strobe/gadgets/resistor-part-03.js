

/*@ numberOfColorBands :: number */
var numberOfColorBands = 4;

/*@ type button = { x         : number
                  , y         : number
                  , downImage : string
                  , image     : string
                  , overImage : string
                  , tooltip   : string
                  , value     : string
                  , color     : string
                  , strikeout : boolean
                  } */

/*@ extern ohms                  :: button */

/*@ extern parseNum :: (string) => number */
 
/*@ containsNonDigit :: (inputStr: string) => { boolean | true } */
function containsNonDigit(inputStr) {
  var stringLength = inputStr.length;
  for (var i = 0; i < stringLength; i++) {
    if ((inputStr.charAt(i) < "0") || (inputStr.charAt(i) > "9")) {
      return (true);
    }
  }

  return (false);
}

/*@ removeCommas :: (inputStr: string) => string */
function removeCommas(inputStr) {
    var noCommasValue = "";
    var index = 0;

    for (var i = 0; i < inputStr.length; i++) {
        if (inputStr.charAt(i) != ",") {
            noCommasValue = noCommasValue + inputStr.charAt(i);
        }
    }

    return (noCommasValue);
}

/*@ doOhmsCheck :: () => { void | true } */
function doOhmsCheck() {
    var cleanedOhms = removeCommas(ohms.value);
    //var minimumOhmsValue = 0, maximumOhmsValue = 0.0;
    var minimumOhmsValue = 0, maximumOhmsValue = 0;
//    if (numberOfColorBands == 4) {
//        minimumOhmsValue = 0;
//        //maximumOhmsValue = 99000000000.0;
//        maximumOhmsValue = 99000000000;
//    } else {
//        minimumOhmsValue = 0;
//        //maximumOhmsValue = 999000000000.0;
//        maximumOhmsValue = 999000000000;
//    }
//
    //if ((cleanedOhms < minimumOhmsValue) || (cleanedOhms > maximumOhmsValue) || (containsNonDigit(cleanedOhms)) || (containsLeadingZero(cleanedOhms)) || (containsErroneousNonZeroDigits(cleanedOhms, numberOfColorBands - 2))) {
//    if ((cleanedOhms < minimumOhmsValue) || (cleanedOhms > maximumOhmsValue) || (containsNonDigit(cleanedOhms))) {
//    //if (cleanedOhms < minimumOhmsValue) {
//        ohms.color = "#FF0000"; // Red.
//        ohms.strikeout = true;
//    } else {
//        ohms.color = "#000000"; // Black.
//        ohms.strikeout = false;
//        resistance = parseNum(cleanedOhms);
//    }
//
//    //doGenerateBandColors();

    return;
}
