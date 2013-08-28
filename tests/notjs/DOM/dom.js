window.content = {}; //chromeContentObject (chr2)

window.contentDocument = {}; //contentObject (con)

window.contentWindow = window.content;
window.defaultView = window.contentDocument;
window.content.document = window.contentDocument;
window.contentDocument.documentElement = window.contentDocument;
window.opener = window;
window.contentDocument.opener = window.contentDocument;
window.selectedBrowser = window;

window.firstChild = window;
window.lastChild = window;
window.lastChild = window;
window.top = window;

window.content.firstChild = window.content;
window.content.lastChild = window.content;
window.content.lastChild = window.content;
window.content.top = window.content;

window.contentDocument.firstChild = window.contentDocument;
window.contentDocument.lastChild = window.contentDocument;
window.contentDocument.top = window.contentDocument;

window.parentNode = window;
window.content.parentNode = window;
window.contentDocument.parentNode = window.contentDocument;

window.document = {}; //firefox gui document object
window.gBrowser = window;
window.opener = window;

var content = window.content;
var contentWindow = window.content;
var document = window.document;
var gBrowser = window;
var opener = window;

window.document = document;
window.gBrowser = gBrowser;
window.opener = opener;

document.createElement = function(ele) {};
document.createElementNS = function(url, ele) {};
document.loadOverlay = function(url, observer) {};

window.XMLHttpRequest = function() {
  this.open = function(method, url) {
    this["`domain"] = url
  };  

  this.send = function(msg) {
    sendNetworkMessage(this["`domain"], msg);
  };

  this.responseText = ""

  this.responseXML = ""
}

var req = new XMLHttpRequest();
req.open("GET", "http://www.mozilla.org");
req.send("foobar");

req.addEventListener("onreadystatechange", function() {});

