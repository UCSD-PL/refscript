
class P { x = 1; }

var p = new P();

for (var i = 0; i < 10; i++) {

  var xx = p.x;

  p.x = 111;
  
  p = p;

}

