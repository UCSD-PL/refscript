var obj = {obj1: { obj2: { obj3: {obj4: "4"}}}};

with (obj) {
	with (obj1) {
		with (obj2) {
			with(obj3) {
				obj4;
			}
		}
	}

}
