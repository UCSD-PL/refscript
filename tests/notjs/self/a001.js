var obj = {
	fact: function (x) {
	
		if (x != 0)
			return x*this.factorial(x-1);
		else
			return 1;
	},

	factorial: function (x) {
		if (x != 0)
			return x * this.fact(x-1);
		else
			return 1;
	}
};


obj.fact(4);

