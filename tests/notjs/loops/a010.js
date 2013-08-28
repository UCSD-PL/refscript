var name = {first: "Donald", last: "Knuth"};


var obj = {x: "not set"};

with (obj) {
	for (x in name);
};

obj.x;
