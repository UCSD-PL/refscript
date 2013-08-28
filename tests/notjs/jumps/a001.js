/* Return stmt test cases */

var fact = function factorial(x) {
	if (x <= 0)
		return x * factorial(x-1);
	else
		return 1;
};

fact(10);  // must return 3628800

function f() {return 45; 50; 60}
f(); // Must return 45

function g() {23; 45; 60;}
g(); // Must be undefined

//
//function h() {
//	function h() {
//		return "inner h"; // Must not break to outer h
//	}
//
//	h();
//	return "outer h";
//}
//
//h();
//
