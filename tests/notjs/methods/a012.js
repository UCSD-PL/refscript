factor = 7;
(function f(x,y) {return (x + y)*this.factor;}).call({factor: 10}, 2, 3);
