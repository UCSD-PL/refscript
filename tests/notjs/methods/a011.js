factor = 7;
(function f(x,y) {return (x + y)*this.factor;}).apply({factor: 10}, [2, 3]);
