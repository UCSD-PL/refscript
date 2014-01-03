
/*@ type color = {number | 0 <= v && v < 16} */

/*@ type band = { downImage : string
                , image     : string
                , overImage : string } */

/*@ extern firstBand  :: band */
/*@ extern secondBand :: band */
/*@ extern thirdBand  :: band */
/*@ extern fourthBand :: band */
/*@ extern fifthBand  :: band */

/*@ buttonStrs :: {[string] | (len v) = 16} */ 
var buttonStrs = [ "0black"
                 , "1brown"
                 , "2red"
                 , "3orange"
                 , "4yellow"
                 , "5green"
                 , "6blue"
                 , "7violet"
                 , "8gray"
                 , "9white"
                 , "Empty"
                 , "Tbrown"
                 , "Tred"
                 , "Tgold"
                 , "Tsilver"
                 , "Blank"];


/*@ currentBandIndex :: {number | 0 <= v && v <= 3} */ 
var currentBandIndex = 0;

/*@ drawNewColorBand :: (color) => void */
function drawNewColorBand(color) {

    switch (currentBandIndex) {
    case 0:
        {
            firstBand.downImage = "stock_images\\Button" + buttonStrs[color] + "Down.PNG";
            firstBand.image = "stock_images\\Button"     + buttonStrs[color] + "Normal.PNG";
            firstBand.overImage = "stock_images\\Button" + buttonStrs[color] + "Over.PNG";
            break;
        }
    case 1:
        {
            secondBand.downImage = "stock_images\\Button" + buttonStrs[color] + "Down.PNG";
            secondBand.image = "stock_images\\Button"     + buttonStrs[color] + "Normal.PNG";
            secondBand.overImage = "stock_images\\Button" + buttonStrs[color] + "Over.PNG";
            break;
        }
    case 2:
        {
            thirdBand.downImage = "stock_images\\Button" + buttonStrs[color] + "Down.PNG";
            thirdBand.image = "stock_images\\Button"     + buttonStrs[color] + "Normal.PNG";
            thirdBand.overImage = "stock_images\\Button" + buttonStrs[color] + "Over.PNG";
            break;
        }
    case 3:
        {
            fourthBand.downImage = "stock_images\\Button" + buttonStrs[color] + "Down.PNG";
            fourthBand.image = "stock_images\\Button"     + buttonStrs[color] + "Normal.PNG";
            fourthBand.overImage = "stock_images\\Button" + buttonStrs[color] + "Over.PNG";
            break;
        }
    }

    return;
}
