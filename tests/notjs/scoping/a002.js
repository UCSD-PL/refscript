var obj = {obj1: { obj2: { obj3: {obj4: "4"}}}};

var foo = function() {
with (obj) {
	with (obj1) {
		with (obj2) {
			with(obj3) {
				try {
					throw "try throwed me";
				}
				catch (obj4 if obj4 == "try throwed me") {
					obj4 = "5";
					return "first catch";
				}
				catch (obj4) {
					return "second catch";
				}
			}
		}
	}

}

};


foo();