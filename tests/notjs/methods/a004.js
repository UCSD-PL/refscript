

x = (function factorial (x) {if (x==0) return 1; return (x*factorial(x-1));})(10);
y = (function g(x) {return x;})(3);
g = function(x) {return 1;};

obj = {f1: function f(x) {return x;}, f2: function(y) {return y;}}
obj.f1(10);
obj.f2(10);

