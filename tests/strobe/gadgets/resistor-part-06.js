
/*@ extern bandNumberValues :: { v:  [{ vv: number | (0 <= vv && vv < 16) }] | (len v) = 5 } */
/*@ extern buttonStrs :: { [string] | (len v) = 16 } */

/*@ view_onOpen :: () => void */
function view_onOpen() {
  var z  = "stock_images\\Button" + buttonStrs[bandNumberValues[0]] + "Down.PNG";
  return;
};
