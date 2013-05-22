    function forloop(lo, hi, body, acc){
      if (lo < hi) {
        var newAcc = body(lo, acc);
        return forloop(lo + 1, hi, body, newAcc);
      }
      return acc;
    }

function minIndex(a){

      requires(length(a) > 0);
      ensures(0 <= $result && $result < (length(a)))
      
      function step(i, min){
        if (a[i] < a[min]) { return i } else { return min }
      }
      
      return forloop(0, length(a), step, 0);
    }



