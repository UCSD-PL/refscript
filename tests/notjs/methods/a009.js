
var factor = 1000;

function f(x,y) {
	return (x + y)*this.factor;
}


var arr = [2,3];
var obj = {factor:10};

f.apply(obj, arr);


