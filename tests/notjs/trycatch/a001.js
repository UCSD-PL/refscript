var c = 0;

function f() {
	try {
		//throw 5;
	}
	catch (e) {
		return e;
	}
}


f(); // undef
