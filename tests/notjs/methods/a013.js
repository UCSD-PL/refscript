
function g() {
	function f() {return 2};

    return f() + (function (){return 4})();
}

g();
