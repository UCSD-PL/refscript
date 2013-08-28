


function foo() {
	var obj = {obj1: {obj2: "field"}};
	function f() {
        with (obj) {
			with (obj1) {
				return obj2.field;
			}
	    }
	}

	return f;
}


foo()(); // must be undefined
