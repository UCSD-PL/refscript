
/*@ kmp_search :: (s:IArray<number>, w:IArray<number>) => {number | true} */
function kmp_search(s:number[], w:number[]) : number {
    var m    = 0;
    var i    = 0; 
    var slen = s.length;
    var wlen = w.length;
             
    /* Construct the lookup table */
    var t   = [-1, 0];
    var pos = 2;
    var cnd = 0; 
    while (pos < wlen) {
      if ( w[pos-1] === w[cnd] ) {
        t[pos] = cnd + 1;
        pos++; cnd++;
      }
      else if ( cnd > 0 )
        cnd = t[cnd];
      else 
        t[pos++] = 0;
      } 
      
      /* Perform the search */
      while (m + i < slen) {
        if (s[m+i] === w[i]) {
          i++;
          if (i === wlen) 
            return m;
        }
        else {
          m += i - t[i];
          if ( t[i] > -1 ) 
            i = t[i];
          else
            i = 0;
        }
      }
      return -1;
  }
