

function f() {
	try {
		throw {f1: 5, f2: "str"};
	}
	catch (obj if obj.no_such_field.dummy.dummy == 45) {
		throw 95;
	}
	catch (obj if obj.f1 == 5 && obj.f2 == "str") {
		throw 100;
	}
	catch (v3) {
		throw 50;
	}
	finally {
		return "finally";
	}
}

f(); //finally
