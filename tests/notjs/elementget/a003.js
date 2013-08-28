var obj = {f1: {f11: "F11"} };

with (obj) {
	f1["f11"] = "modified F11";
};

obj["f1"]["f11"];
