
/*@ containsErroneousNonZeroDigits :: (inputStr: string, zerosStartAtPosition: number) => { boolean | true } */
function containsErroneousNonZeroDigits(inputStr, zerosStartAtPosition) {
    var length = inputStr.length;

    if (length <= zerosStartAtPosition) {
        return (false);
    }

    for (var i = zerosStartAtPosition; i < length; i++) {
        if (inputStr.charAt(i) != "0") {
          return (true);
        }
    }

    return (false);
}

