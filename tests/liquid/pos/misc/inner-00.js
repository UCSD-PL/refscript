
/*@ main :: (x:number) => { v:number |v > x} */
function main(x){

  var rand = 10;
  if (rand > 5){
    rand = rand + 1;
  }

  /*@ plus :: (number) => number */
  function plus(a){ 
    return a + x 
  };
  
  var z = plus(12);

  return z;
}
 
