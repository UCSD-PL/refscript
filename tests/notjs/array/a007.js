
var x = Array(6);

for (var i = 0;i < 6;i++) { 
  x[i] = Array(i,i+1,i+2);
}

x[3][2]; //should be 5


