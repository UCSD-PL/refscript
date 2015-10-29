/*@ qualif Locked(v:number): v != 0   */    
/*@ qualif Unlocked(v:number): v = 0  */    
/*@ qualif CmpO(v:number): v >  1 */
/*@ qualif One(v:number): v = 1 */

// Only one of the two below is needed...

/*@ qualif CondLock1(v:number,x:number): v = (if (0 < x) then 1 else 0)  */    
/*@ qualif CondLock2(v:number,x:number): ((0 < x) <=> (v = 0))  */    

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

/*@ loop :: (number, number) => number */
function loop(n:number, l:number):number {
	let flag :number= random();
	if (0 < n){
		if (0 < flag){ 
			l = acquire(l); 
		}
		if (0 < flag){ 
			l = release(l);
		}
		loop(n-1, l);
	}
	return l;
}

/*@ main :: ({n:number|n > 0}) => void */
function main(n:number):void{
	let flag :number= random();
	let l:number= create();
	loop(n, l);
	assert(l === 0);
}

