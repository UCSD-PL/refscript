/*@ qualif CondLock1(v:number,x:number): v = ((0 <= x) ? 1 : 0)  */    

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



/*@ main :: () => { void | true } */
function main():void{
	var x :number = random();
	var l :number = create();
	if (0 <= x){ l = acquire(l); }
	if (0 <= x){ l = release(l); }
	assert(l === 0);
}

