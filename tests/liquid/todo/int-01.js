
/*@ simple :: (x: number) => { v:number | (v != 0) } */
function simple(x) {

  if (x) 
    return x;

  return 1;


}
