var obj = {f1: 8};

var x = 2;

with(obj) {
	i = x++;  
    j = ++f1;
}

// i must be 2
// x must be 3
// j must be 9
// f1 must be 9

(i + "") + (x + "") + (j + "") + (obj.f1 + "");
