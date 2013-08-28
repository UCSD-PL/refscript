// expected value: bottom, private 
var p = lSLoV("foo", "private");
var obj = {};
obj[p] = 1;
var z = obj["bar"];
z
