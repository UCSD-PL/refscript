
/*@ qualif CondLock2(v: int,x: int,y: int): ((x != y) <=> (v = 0))  */

function create() : number{
  return 0;
}

function acquire(l: number) {
    assert(l === 0);
    return 1;
}

function release(l: number) {
    assert(l !== 0);
  return 0;
}

function driver(l: number, newCount: number, oldCount: number) {
	if (newCount !== oldCount){
		l = acquire(l);
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

export function main(): void {
	let newCount = pos();
	let oldCount = pos();
	let l = create();
	if (newCount < oldCount) {
		l = driver(l, newCount, oldCount);
        l = release(l);
	}
}
