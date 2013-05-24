

// BUG: Why is it not inferring the type?

/* abs :: (({z:int| z >= 0}) => {v:int | v >= 0}, int) => {v:int|v >= 0} */ 

/*@ abs :: ((int) => int, int) => int */ 
function abs(f, x){
  var r = x;
  if (x < 0){
    r = 0 - x;
  } 
  r = f(r);
  assert(r >= 0);
  return r;
}

/*@ dubble :: ({p:int | p >= 0}) => {v:int | v >= p} */
function dubble(p){ return p + p }

/*@ main :: (int) => {v:int | v >= 0 } */
function main(y){
  var y = abs(dubble, y);
  assert(y >= 0);
  return y;
}

// p>=0  <: K?
// v>=p  <: K4
// 
// -----------------------------
// p>=0 => v >= p  <:   K? => K4
