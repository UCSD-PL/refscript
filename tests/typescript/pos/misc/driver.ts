/*@ qualif Locked(v:number): v != 0   */    
/*@ qualif Unlocked(v:number): v = 0  */    
/*@ qualif CondLock2(v:number,x:number,y:number): ((x != y) <=> (v = 0))  */    

// HINT: Recall the `invariant` for the corresponding test in tests/esc/pos
//       Find a way to represent that invariant as a qualifier (or a conjunction of qualifiers.)
//       You can use operators like <=>, =>, &&, || in the qualifiers.

/*@ create :: () => number */
function create() : number{
  return 0;
}

/*@ acquire :: (number) => number */
function acquire(l:number):number{
  assert(l == 0);
  return 1;
}

/*@ release :: (number) => number */
function release(l:number):number{
  assert(l != 0);
  return 0;
}

/*@ driver :: (number, number, number) => number */ 
function driver(l:number, newCount:number, oldCount:number){
	if (newCount != oldCount){
		l        = acquire(l);
		oldCount = newCount;
		if (0 < newCount){
			l = release(l);
			newCount = newCount - 1;
		} else {
			newCount = newCount;
		}
		l = driver(l, newCount, oldCount);
	}
	return l;
}

/*@ main :: () => void */
function main():void {
	var newCount = pos();
	var oldCount = pos(); 
	var l :number= create();
	if (newCount < oldCount) {
		l = driver(l, newCount, oldCount); 
		l = release(l);
	}
}

