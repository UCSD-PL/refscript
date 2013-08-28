/* Basic higher order test */

function f() {

	function g(x) {
		return x*x;
	}
	return g;
}

f()(4); // Must return 16


function g(singleArgFunc) {
   return singleArgFunc(4)
}

function square(x) { return x * x}

g(square); // Must return 16

