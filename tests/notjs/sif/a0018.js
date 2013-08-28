//expected output: public, private

function foo() {
  var x = lSLoV(UNKNOWN_NUMBER, "private");
  if ((x+1) == 12)
    return {};
  else 
    return {"a": 1};
}

var obj = foo();
var result = obj.a;
result;
 
