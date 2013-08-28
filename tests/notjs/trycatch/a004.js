
function f() {
	try {
		throw {f1: 5, f2: "str"};
	}
	catch (obj if obj.f1 == 45) {
		return "caught in v1";
	}
	catch (obj if obj.f1 == 5 && obj.f2 == "str") {
		return "caught in v2";
	}
	catch (v3) {
		return "caught in v3";
	}
}

f();
