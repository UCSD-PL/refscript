var i = 1;

s = "";

switch(i) {
	case 5: var s = s + "case 5;"; // s must be hoisted
	case 1: s = s + "case 1;";
}

s; // "case 1;"