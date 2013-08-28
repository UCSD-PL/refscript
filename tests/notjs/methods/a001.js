
function f(x) {
	return x*x;
}

var g = function(x) {return f(x);};

var fact = function factorial(x) {
	if (x == 0)
		return 1;

	else if (x==1) {  //Intentional. 
		return x * factorial(x-1);
	}
	else {
		return x * fact(x-1);
	}
};

fact(g(2)); // 24

