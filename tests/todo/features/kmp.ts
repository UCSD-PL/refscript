/*@ newArray :: forall T.(n:number, v:T) => {IArray<T> | len v = n} */
function newArray<T>(n:number, v:T): T[] {
    return newArray(n, v);
}

/*@ qualif LT(v:number, x:number): v < x */
/*@ alias nat = {v:number | 0 <= v} */

/*@ kmp_table_rec :: (p:IArray<number>) => {IArray<nat> | len(v) = len(p)}  */
function kmp_table_rec(p) {
   var m    = p.length;

   /*@ loop :: (number, number, IArray<number>) => IArray<number> */
   function loop(i, j, next){ 
	if (i < m - 1) { 
	    assume (j < i);
	    if (p[i] === p[j]) {
		i++; 
		j++; 
		next[i] = j; 
	    } 
	    else if (j === 0) { 
                i++; 
		next[i] = 0; 
            } 
	    else { 
		j = next[j];
	    }
	    return loop(i, j, next);
	}
	return next;
    }
    var next = newArray(m, 0);
    var i    = 1;
    var j    = 0; 
    return loop(i, j, next);
}

/*@ kmp_table :: (p:IArray<number>) => {IArray<{v:number | 0 <= v}> | len(v) = len(p)}  */
function kmp_table(p) {
    var m    = p.length;
    var next = newArray(m, 0);
    var i    = 1;
    var j    = 0; 
    while (i < m - 1) { 
	// NEEDS INVARIANT: forall 0 <= i < next.length. next[i] <= i 
	// Which you can get via Abs-Ref.
	assume (j < i);
	if (p[i] === p[j]) {
            i++; 
            j++; 
            next[i] = j; 
	} 
	else if (j === 0) { 
            i++; 
            next[i] = 0; 
        } 
	else { 
            j = next[j];
	}
    }
    return next;
}



/*@ kmp_search :: (p:IArray<number>, s:IArray<number>) => {number | 0 < 1} */ 
function kmp_search(p, s) {
  var next = kmp_table(p);
  var m    = p.length;
  var n    = s.length; 
  var i    = 0;
  var j    = 0; 
  while (j < m && i < n) { 
      if (s[i] === p[j]) { 
	  i++; 
	  j++; 
      } 
      else if (j === 0) { 
	  i++;
      } 
      else {
	  j = next[j];
      }
  }
  var res = -1;
  if (j >= m) { 
      res = i - m;
  }
  return res;
} 
