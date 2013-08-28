
var factor = 10;

function f(x,y) {
	return (x + y)*this.factor;
}

var obj = {factor: 1000};

f.call(obj, 2, 3);
