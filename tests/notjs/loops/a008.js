
obj = {f1: 2, f2: 3, f3: 5, f4: 7};

var product = 1;

for (x in obj) {
	product *= obj[x];
};

product;
