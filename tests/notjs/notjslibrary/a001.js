
var str = "";

if ("") {str += "true;"} else {str += "false;"}

do {str += "do while entered;"} while (null);

while("abcd") {
	str += "while entered;";
	break;
}

for (i=2; i; i--) {
}

if (7) { str += "7;" } else { str += "FAIL"; }

if ({f:1}) { str += "f1;" } else { str += "FAIL"; }

if (+0.0) str += "true;"; else str += "false;";
if (-0.0) str += "true;"; else str += "false;";

v = 5;
(v) ?  (str += "true;") : (str += "false;");

str;

//"false;do while entered;while entered;7;f1;false;false;true;"