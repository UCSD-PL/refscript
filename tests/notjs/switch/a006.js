var i = 5;

s = "";
while (true) {
  switch(i) {
	case 5:{ 
       s = s + "case 5;"; // s must be hoisted
       do {
           s = s + "do-while;";
	   } while(false);
	   break;
	}
	case 1: s = s + "case 1;";
  }
  s = s + "just before while break";
  break;
}
s; // "case 5; do-while; just before while break"