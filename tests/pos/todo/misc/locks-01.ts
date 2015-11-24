/*@ qualif CondLock1(v:number,x:number): v = (if (0 <= x) then 1 else 0)  */    

/*@ create :: () => number */
function create():number{
  //ensures($result === 0);
  return 0;
}

/*@ acquire :: (number) => number */
function acquire(l:number):number{
  //requires(l === 0);
  //ensures($result === 1);
  return 1;
}

/*@ release :: (number) => number */
function release(l:number):number{
  //requires(l === 1);
  //ensures($result === 0);
  return 0;
}



/*@ main :: () => { void | 0 < 1 } */
function main():void{
	let x :number = random();
	let l :number = create();
	if (0 <= x){ l = acquire(l); }
	if (0 <= x){ l = release(l); }
	assert(l === 0);
}

