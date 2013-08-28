var x=1;
var y=0;
if (true) {
    var x=2;
    if (true) {
        var x=3;
        y = y + x;
    }
    y = y + x;
}
y = y + x;
y //should be 9!
