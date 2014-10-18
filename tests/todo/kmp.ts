function kmp_search(s, w) {
  var m = 0, i = 0, 
          pos, cnd, t,
          slen = s.length,
          wlen = w.length;
      
      /* String to array conversion */
      s = s.split("");
      w = w.split("");    
              
      /* Construct the lookup table */
      t = [-1, 0];
      for ( pos = 2, cnd = 0; pos < wlen; ) {
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
      while ( m + i < slen ) {
          if ( s[m+i] === w[i] ) {
              i++;
              if ( i === wlen ) 
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
