
/*@ abs :: ((number) => number, number) => number */ 
function abs(f, x){
  let r = x;
  if (x < 0){
    r = 0 - x;
  } 
  r = f(r);
  assert(r >= 0);
  return r;
}

/*@ dubble :: ({p:number | p >= 0}) => {v:number | v >= p} */
function dubble(p){ return p + p }

/*@ main :: (number) => {v:number | v >= 0 } */
function main(y){
  let y = abs(dubble, y);
  assert(y >= 0);
  return y;
}

// p>=0  <: K?
// v>=p  <: K4
// 
// -----------------------------
// p>=0 => v >= p  <:   K? => K4
