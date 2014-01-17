
/*@ bandNumberValues :: [{ number | (0 <= v && v < 16) }] */
var bandNumberValues =  [1, 0, 2, 10, 15]; // Brown, Black, Red, Empty, Blank.

/*@ numberOfColorBands :: { number | v < 5 } */
var numberOfColorBands = 4;

/*@ doCalculateResistance :: () => void */
function doCalculateResistance() {
  var currentBandValue=0;
  var power=0;

  resistance = 0;
  for (var bandIndex /*@ { number | v < 2 } */ = 0; bandIndex < numberOfColorBands - 2; bandIndex++) {
    currentBandValue = bandNumberValues[bandIndex];
  }
  return;
}

