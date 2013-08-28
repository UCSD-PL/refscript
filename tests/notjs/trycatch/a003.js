

function f() {
	try {
		throw "try throwed";
	}
	catch (v1 if v1 == 5) {
		return "caught in v1";
	}
	catch (v2 if v2 == "try throwed") {
		return "caught in v2";
	}
	catch (v3) {
		return "caught in v3";
	}
}

f(); // caught in v2
