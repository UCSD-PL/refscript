window.XMLHttpRequest = function() {
  this.open = function(method, url) {
    this["`domain"] = url;
    this.responseText = lSLoV("", this["`domain"]); //lSLoV
    this.responseXML = lSLoV("", this["`domain"]); //lSLoV
  };  

  this.send = function(msg) {
    sendNetworkMessage(this["`domain"], msg);
  };
}

var req = new XMLHttpRequest();
req.open("GET", "http://www.mozilla.org");
req.send("foobar");

var anotherReq = new XMLHttpRequest(); 
anotherReq.open("POST", "www.ucsb.edu");
anotherReq.send("hello");

req.responseText + anotherReq.responseText;
