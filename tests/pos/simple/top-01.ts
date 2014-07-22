
/*@ resistance :: number */
var resistance :number = 1000;

/*@ makeNum :: (x: top) => number */ 
function makeNum(x):number {
  return 1;
}


/*@ doCalculateResistance :: () => void */
function doCalculateResistance():void {
  resistance = makeNum(1) ;
}
