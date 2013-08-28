var obj = {f1: 1, f2: 2, f3: 3};

var result = ""

if ("f1" in obj)
	result += "yes;";
else
	result += "no;";

if (3 in obj)
	result += "yes;";
else
	result += "no;";

result;
