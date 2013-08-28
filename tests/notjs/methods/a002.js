
var obj  = {
	g:	function () {
		this.k = 1;	
	},
	fact: function factorial(x) {
		if (x == 0) {
			return 1;
		}
		else {
			return x * factorial(x-1);
		}
	}
};


obj.g();


window.obj.fact(4) + obj.k;

