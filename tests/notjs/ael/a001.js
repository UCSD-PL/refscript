addEventListener("onclick", function(evt) { y = 42; }, false); 
bar = function(evt) { y = 42; };
window.removeEventListener("onclick", bar, false);

var x = {z: {}};
x.z.addEventListener("onclick", bar);

x.z.dispatchEvent("blah");

sendNetworkMessage("mozilla.com", "huha");

