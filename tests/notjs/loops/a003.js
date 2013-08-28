
var j = "-";

for (var i=0; i < 10; i = i + 1) {

    if (i < 5)
	{
		j = j + "@";
		continue;
	}


	j = j + "#";
}
j; // should be:  "-@@@@@#####"

