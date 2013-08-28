
var factor = 10;

function f(x,y) {
	return (x + y)*this.factor;
}

//var obj = {factor: 1000};

f.call(this, 2, 3);
