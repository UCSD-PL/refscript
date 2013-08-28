/* Continue test */
var flag = true;
var str = "";

l1:while (flag) {
	l2:while (flag) {
		l3:while(flag) {
			l4:while (flag) {
                str = "PASS";
				break; // Must leave l4
                str = "FAILED";
			}
			break l2;
            str = "FAILED";
		}
        str = "FAILED";
	}
    break l1;
    str = "FAILED";
}

str; // Must be PASS