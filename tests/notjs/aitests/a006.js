//man or boy test

function A(k, x1, x2, x3, x4, x5) { 
    function B() { return A(--k, B, x1, x2, x3, x4); }
    if (k <= 0) 
      return x4() + x5();
    else 
      return B();
}
 
function K(n) { return function() { return n} };
 
A(6, K(1), K(-1), K(-1), K(1), K(0));
