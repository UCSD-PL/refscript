/* Continue test */
var flag = true;

outer:
while(flag) {
	inner:
	while (true) {
		flag = false;
		continue outer;
		flag = true;
	}
}


flag; // Must be false
