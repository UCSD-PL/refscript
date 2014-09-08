// Taken from strobe

/*@ foo :: (obj: { [x:string]: string + number }) => string */ 
function foo(obj) {
  for (var x in obj) {
    var r = obj[x];
    if (typeof r === "string") {
      return r;
    }
  }
  return "no string found";
};
