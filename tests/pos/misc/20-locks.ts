/*@ qualif Locked(v: int): v != 0    */
/*@ qualif Unlocked(v: int): v = 0  */    

/*@ create :: () => number */
function create():number{
  return 0;
}

/*@ acquire :: (number) => number */
function acquire(l:number):number{
  assert(l === 0);
  return 1;
}

/*@ release :: (number) => number */
function release(l:number):number{
  assert(l !== 0);
  return 0;
}

/*@ work :: () => void */
function work():void{
  return;
}

/*@ loop :: (number, number) => number */
function loop(n:number, l:number) {

  let flag :number= random();
  if (n <= 0) {
    return l;
  }

  l = acquire(l);
  work();
  l = release(l);

  return loop(n-1, l);
}


/*@ main :: ({n:number|n > 0}) => void */
function main(n){
  let l :number= create();
  l = loop(n, l);
  assert(l === 0);
}
