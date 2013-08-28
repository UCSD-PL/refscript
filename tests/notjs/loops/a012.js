

function foo() {
	var count = 0;
	for (arg in arguments) {
		count++;
	}
	return count;
}

foo();
