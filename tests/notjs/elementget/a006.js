var obj1 = {f1: "f1", f2: "f2"};
var obj2 = {str: "f1"};

with (obj2) {
	obj1[str] = "modified";
}

obj1.f1;

