// expected value: bottom, private 
var p = lSLoV("foo", "private");
var obj = {"blah": 1};
delete obj[p];
var z = obj["bar"];
z
