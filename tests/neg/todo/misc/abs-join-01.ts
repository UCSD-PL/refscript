
/*@ abs :: ({ } + number) => { boolean | 0 < 1 } */ 

function abs(x: any) {

  return 0 > x;

}

