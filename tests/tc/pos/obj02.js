/*@ getNum ::  ({}   ) => number */

function getNum(x) {

  if (x && x.a && typeof(x.a) == 1)
    return x.a;
  
  return -1;

}

