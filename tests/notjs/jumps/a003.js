

var i = 0;

function f() {
for (; function(){return true}();) {
	window.i = 5;
	break;
}

}

f();

i;
