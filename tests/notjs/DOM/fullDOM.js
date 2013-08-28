// This file is derived from domcore.idl file from dom.js project.

var event = {
  type:"sdf",                   //readonly attribute DOMString type;
  taget:eventTarget,            //readonly attribute EventTarget? target;
  currentTarget:eventTarget,   //readonly attribute EventTarget? currentTarget;

  CAPTURING_PHASE:1,
  AT_TARGET:2,
  BUBBLING_PHASE:3,
  eventPhase:1, //readonly attribute unsigned short eventPhase;

  stopPropagation:function(){},
  stopImmediatePropagation:function(){},

  bubbles:true,
  cancelable:true,
  preventDefault:function(){},
  defaultPrevented:true,    //readonly attribute boolean defaultPrevented;

  isTrusted:true,    //readonly attribute boolean isTrusted;
  timeStamp:"TODO",    //readonly attribute DOMTimeStamp timeStamp;

  initEvent:function(type, bubbles, cancelable){} //void initEvent(DOMString type, boolean bubbles, boolean cancelable);
}

/*
dictionary EventInit {
  boolean bubbles;
  boolean cancelable;
};

[Constructor(DOMString type, optional CustomEventInit eventInitDict)]
interface CustomEvent : Event {
  detail:,    //readonly attribute any detail;
};

dictionary CustomEventInit : EventInit {
  any detail;
};
*/

var eventTarget = {
  addEventListener:function(type, listener, capture) {}, //void addEventListener(DOMString type, EventListener? listener, optional boolean capture);
  removeEventListener:function(type, listener, capture) {}, //void removeEventListener(DOMString type, EventListener? listener, optional boolean capture);
  dispatchEvent:function(event) {return true;} //boolean dispatchEvent(Event event);
}

var eventListener = {
  handleEvent:function(event){} //void handleEvent(Event event);
}

var node = {
  prototype:eventTarget,
  ELEMENT_NODE:1,    //const unsigned short ELEMENT_NODE = 1;
  ATTRIBUTE_NODE:2,    //const unsigned short ATTRIBUTE_NODE = 2; // historical
  TEXT_NODE:3,    //const unsigned short TEXT_NODE = 3;
  CDATA_SECTION_NODE:4,    //const unsigned short CDATA_SECTION_NODE = 4; // historical
  ENTITY_REFERENCE_NODE:5,    //const unsigned short ENTITY_REFERENCE_NODE = 5; // historical
  ENTITY_NODE:6,    //const unsigned short ENTITY_NODE = 6; // historical
  PROCESSING_INSTRUCTION_NODE:7,    //const unsigned short PROCESSING_INSTRUCTION_NODE = 7;
  COMMENT_NODE:8,    //const unsigned short COMMENT_NODE = 8;
  DOCUMENT_NODE:9,    //const unsigned short DOCUMENT_NODE = 9;
  DOCUMENT_TYPE_NODE:10,    //const unsigned short DOCUMENT_TYPE_NODE = 10;
  DOCUMENT_FRAGMENT_NODE:11,    //const unsigned short DOCUMENT_FRAGMENT_NODE = 11;
  NOTATION_NODE:12,    //const unsigned short NOTATION_NODE = 12; // historical
  nodeType:1,    //readonly attribute unsigned short nodeType;
  nodeName:"head",    //readonly attribute DOMString nodeName;

  baseURI:"about: blank",    //readonly attribute DOMString baseURI;

  ownerDocument:document,    //readonly attribute Document? ownerDocument;
  parentNode:node,    //readonly attribute Node? parentNode;
  parentElement:element,    //readonly attribute Element? parentElement;
  hasChildNodes:function(){return true}, //boolean hasChildNodes();
  childNodes:"TODO",    //readonly attribute NodeList childNodes;
  firstChild:node,    //readonly attribute Node? firstChild;
  lastChild:node,    //readonly attribute Node? lastChild;
  previousSibling:node,    //readonly attribute Node? previousSibling;
  nextSibling:node,    //readonly attribute Node? nextSibling;

  DOCUMENT_POSITION_DISCONNECTED:0x01,    //const unsigned short DOCUMENT_POSITION_DISCONNECTED = 0x01;
  DOCUMENT_POSITION_PRECEDING:0x02,    //const unsigned short DOCUMENT_POSITION_PRECEDING = 0x02;
  DOCUMENT_POSITION_FOLLOWING:0x04,    //const unsigned short DOCUMENT_POSITION_FOLLOWING = 0x04;
  DOCUMENT_POSITION_CONTAINS:0x08,    //const unsigned short DOCUMENT_POSITION_CONTAINS = 0x08;
  DOCUMENT_POSITION_CONTAINED_BY:0x10,    //const unsigned short DOCUMENT_POSITION_CONTAINED_BY = 0x10;
  DOCUMENT_POSITION_IMPLEMENTATION_SPECIFIC:0x20,    //const unsigned short DOCUMENT_POSITION_IMPLEMENTATION_SPECIFIC = 0x20;
  compareDocumentPosition:function(node){return 0x01;}, //unsigned short compareDocumentPosition(Node other);

  nodeValue:"NoValue",          //attribute DOMString? nodeValue;
  textContent:"text",           //attribute DOMString? textContent;
  insertBefore:function(a1, a2){if (a1 && a2) return this; else return this;} //Node insertBefore(Node newChild, Node? refChild);
  replaceChild:function(a1, a2){if (a1 && a2) return this; else return this;} //Node replaceChild(Node newChild, Node oldChild);
  removeChild:function(a){return a;} //Node removeChild(Node oldChild);
  appendChild:function(a){return a;} //Node appendChild(Node newChild);

  cloneChild:function(a){return a;} //Node cloneNode(boolean deep);
  isSameNode:function(a){if (a) return true; else return false;} //boolean isSameNode(Node? node);
  isEqualNode:function(a){if (a) return true; else return false;} //boolean isEqualNode(Node? node);

  lookupPrefix:function(a){return a;} //DOMString lookupPrefix([TreatNullAs=EmptyString] DOMString namespace);
  lookupNamespaceURI:function(a){return a;} //DOMString lookupNamespaceURI(DOMString? prefix);
  isDefaultNamespace:function(a){return a > 0;} //boolean isDefaultNamespace([TreatNullAs=EmptyString] DOMString namespace);
}

var documentFragment = {
  prototype:node
}

var document = {
  prototype:node,

  implementation:domImplementation,    //readonly attribute DOMImplementation implementation;
  documentURI:"asfa",         //attribute DOMString documentURI;
  compatMode:"saf",    //readonly attribute DOMString compatMode;

  doctype:documentType,    //readonly attribute DocumentType? doctype;
  documentElement:element,    //readonly attribute Element? documentElement;
  getElementByTagName:function(a){if (a) return nodeList; else return nodeList;}, //NodeList getElementsByTagName(DOMString qualifiedName);
  getElementsByTagNameNS:function(a1, a2){ if (a1 && a2) return nodeList; else return nodeList;}, //NodeList getElementsByTagNameNS(DOMString namespace, DOMString localName);
  getElementsByClassName:function(a){if (a) return nodeList; else return nodeList;}, //NodeList getElementsByClassName(DOMString classNames);
  getElementById:function(id){if(id) return element; else return element;}, //Element? getElementById(DOMString elementId);

  createElement:function(a) {if (a) return element; else return element;}, //Element createElement([TreatNullAs=EmptyString] DOMString localName);
  createElementNS:function(a1, a2) {if (a1 && a2) return element; else return element;}, //Element createElementNS(DOMString namespace, DOMString qualifiedName);
  createDocumentFragment:function(){return documentFragment;}, //DocumentFragment createDocumentFragment();
  createTextNode:function(a){if (a) return text;}, //Text createTextNode(DOMString data);
  createComment:function(a){if (a) return characterData; else return characterData;}, //Comment createComment(DOMString data);
  createProcessingInstruction:function(a1, a2){if (a1 && a2) return processingInstruction; else return processingInstruction;},  //ProcessingInstruction createProcessingInstruction(DOMString target, DOMString data);

  importNode:function(a1, a2){ if (a1 && a2) return a2; else return a2;},  //Node importNode(Node node, boolean deep);
  adoptNode:function(a1){return a1;}, //Node adoptNode(Node node);

  createEvent:function(a){if(a) return event; else return event;}, //Event createEvent(DOMString eventInterfaceName);


//
// The HTML spec says that all documents have the following members.
// Currently HTML says "Document implements HTMLDocument", but that may
// change to "partial interface Document", so I'm putting them directly
// in the Document interface here.  Note that many of these HTML-defined
// members are commented out for now.
//

  // resource metadata management

  // XXX: this is breaking jstests.py, so it is commented out for now
  // [PutForwards=href] readonly attribute Location? location;

  URL:"af",    //readonly attribute DOMString URL;
  domain:"asd", //attribute DOMString domain;
  referrer:"asf",    //readonly attribute DOMString referrer;
  cookie:"af",         //attribute DOMString cookie;
  lastModified:"asdf",    //readonly attribute DOMString lastModified;
  readyState:"asfas",    //readonly attribute DOMString readyState;

  // DOM tree accessors
  // XXX The getter is commented out for now, since
  // I'm not ready to write DocumentProxy yet.
  //  getter any (in DOMString name);

  title:"af",         //attribute DOMString title;
  dir:"asf",         //attribute DOMString dir;

  body:htmlElement,         //attribute HTMLElement? body;
  head:htmlElement,    //readonly attribute HTMLHeadElement? head;
  images:htmlCollection,    //readonly attribute HTMLCollection images;
  embeds:htmlCollection,    //readonly attribute HTMLCollection embeds;
  plugins:htmlCollection,    //readonly attribute HTMLCollection plugins;
  links:htmlCollection,    //readonly attribute HTMLCollection links;
  forms:htmlCollection,    //readonly attribute HTMLCollection forms;
  scripts:htmlCollection,    //readonly attribute HTMLCollection scripts;
  getElementsByName:function(a1){ if (a1) return nodeList; else return nodeList;}, //NodeList getElementsByName(in DOMString elementName);
  //  NodeList getItems(in optional DOMString typeNames); // microdata
  //  readonly attribute DOMElementMap cssElementMap;

  // dynamic markup insertion
  innerHTML:"adf",         //attribute DOMString innerHTML;

  //  HTMLDocument open(in optional DOMString type, in optional DOMString replace);
  //  WindowProxy open(in DOMString url, in DOMString name, in DOMString features, in optional boolean replace);
  //  void close();
  write:function(a){}, //void write(in DOMString... text);
  writeIn:function(a){}, //void writeln(in DOMString... text);

  // user interaction
  defaultView:window,    //readonly attribute Window? defaultView;

  //  readonly attribute Element? activeElement;
  //  boolean hasFocus();
  //           attribute DOMString designMode;
  //  boolean execCommand(in DOMString commandId);
  //  boolean execCommand(in DOMString commandId, in boolean showUI);
  //  boolean execCommand(in DOMString commandId, in boolean showUI, in DOMString value);
  //  boolean queryCommandEnabled(in DOMString commandId);
  //  boolean queryCommandIndeterm(in DOMString commandId);
  //  boolean queryCommandState(in DOMString commandId);
  //  boolean queryCommandSupported(in DOMString commandId);
  //  DOMString queryCommandValue(in DOMString commandId);
  //  readonly attribute HTMLCollection commands;

  // event handler IDL attributes
  onabort:function(){},    //attribute Function? onabort;
  onblur:function(){},    //attribute Function? onblur;
  oncanplay:function(){},    //attribute Function? oncanplay;
  oncanplaythrough:function(){},    //attribute Function? oncanplaythrough;
  onchange:function(){},    //attribute Function? onchange;
  onclick:function(){},    //attribute Function? onclick;
  oncontextmenu:function(){},    //attribute Function? oncontextmenu;
  oncuechange:function(){},    //attribute Function? oncuechange;
  ondblclick:function(){},    //attribute Function? ondblclick;
  ondrag:function(){},    //attribute Function? ondrag;
  ondragend:function(){},    //attribute Function? ondragend;
  ondragenter:function(){},    //attribute Function? ondragenter;
  ondragleave:function(){},    //attribute Function? ondragleave;
  ondragover:function(){},    //attribute Function? ondragover;
  ondragstart:function(){},    //attribute Function? ondragstart;
  ondrop:function(){},    //attribute Function? ondrop;
  ondurationchange:function(){},    //attribute Function? ondurationchange;
  onemptied:function(){},    //attribute Function? onemptied;
  onended:function(){},    //attribute Function? onended;
  onerror:function(){},    //attribute Function? onerror;
  onfocus:function(){},    //attribute Function? onfocus;
  oninput:function(){},    //attribute Function? oninput;
  oninvalid:function(){},    //attribute Function? oninvalid;
  onkeydown:function(){},    //attribute Function? onkeydown;
  onkeypress:function(){},    //attribute Function? onkeypress;
  onkeyup:function(){},    //attribute Function? onkeyup;
  onload:function(){},    //attribute Function? onload;
  onloadeddata:function(){},    //attribute Function? onloadeddata;
  onloadedmetadata:function(){},    //attribute Function? onloadedmetadata;
  onloadstart:function(){},    //attribute Function? onloadstart;
  onmousedown:function(){},    //attribute Function? onmousedown;
  onmousemove:function(){},    //attribute Function? onmousemove;
  onmouseout:function(){},    //attribute Function? onmouseout;
  onmouseover:function(){},    //attribute Function? onmouseover;
  onmouseup:function(){},    //attribute Function? onmouseup;
  onmousewheel:function(){},    //attribute Function? onmousewheel;
  onpause:function(){},    //attribute Function? onpause;
  onplay:function(){},    //attribute Function? onplay;
  onplaying:function(){},    //attribute Function? onplaying;
  onprogress:function(){},    //attribute Function? onprogress;
  onratechange:function(){},    //attribute Function? onratechange;
  onreadystatechange:function(){},    //attribute Function? onreadystatechange;
  onreset:function(){},    //attribute Function? onreset;
  onscroll:function(){},    //attribute Function? onscroll;
  onseeked:function(){},    //attribute Function? onseeked;
  onseeking:function(){},    //attribute Function? onseeking;
  onselect:function(){},    //attribute Function? onselect;
  onshow:function(){},    //attribute Function? onshow;
  onstalled:function(){},    //attribute Function? onstalled;
  onsubmit:function(){},    //attribute Function? onsubmit;
  onsuspend:function(){},    //attribute Function? onsuspend;
  ontimeupdate:function(){},    //attribute Function? ontimeupdate;
  onvolumechange:function(){},    //attribute Function? onvolumechange;
  onwaiting:function(){}    //attribute Function? onwaiting;
}

var domImplementation = {
  hasFeature:function(a1, a2){return a1 && a2;}, //boolean hasFeature(DOMString feature, [TreatNullAs=EmptyString] DOMString version);

  createDocumentType:function(a1, a2, a3){if (a1 && a2 && a3) return documentType; else return documentType;}, //DocumentType createDocumentType([TreatNullAs=EmptyString] DOMString qualifiedName, DOMString publicId, DOMString systemId);
  createDocument:function(a1, a2, a3){if (a1 && a2 && a3) return document; else return document;}, //Document createDocument([TreatNullAs=EmptyString] DOMString namespace, [TreatNullAs=EmptyString] DOMString qualifiedName, DocumentType? doctype);
  createHTMLDocument:function(a1){if (a1) return document; else return document;}, //Document createHTMLDocument(DOMString title);
}

var element {
  prototype:node,
  namespaceURI:"af",    //readonly attribute DOMString? namespaceURI;
  prefix:"asf",    //readonly attribute DOMString? prefix;
  localName:"asf",    //readonly attribute DOMString localName;
  tagName:"af",    //readonly attribute DOMString tagName;

  attributes:[attr],    //readonly attribute Attr[] attributes;
  getAttribute:function(a){return a;}, //DOMString? getAttribute(DOMString qualifiedName);
  getAttributeNS:function(a1, a2){return a1+ a2;}, //DOMString? getAttributeNS(DOMString namespace, DOMString localName);
  setAttribute:function(a1, a2){}, //void setAttribute(DOMString qualifiedName, DOMString value);
  setAttributeNS:function(a1, a2, a3){}, //void setAttributeNS(DOMString namespace, DOMString qualifiedName, DOMString value);
  removeAttribute:function(a1){}, //void removeAttribute(DOMString qualifiedName);
  removeAttributeNS:function(a1, a2){}, //void removeAttributeNS(DOMString namespace, DOMString localName);
  hasAttribute:function(a1){return a1 > 0;}, //boolean hasAttribute(DOMString qualifiedName);
  hasAttributeNS:function(a1, a2){return a1 > a2;}, //boolean hasAttributeNS(DOMString namespace, DOMString localName);

  getElementsByTagName:function(a1){if (a1) return nodeList; else return nodeList;}, //NodeList getElementsByTagName(DOMString qualifiedName);
  getElementByTagNameNS:function(a1, a2){if (a1 && a2) return nodeList; else return nodeList;}, //NodeList getElementsByTagNameNS(DOMString namespace, DOMString localName);
  getElementsByClassName:function(a1){if (a1) return nodeList; else return nodeList;}, //NodeList getElementsByClassName(DOMString classNames);

  children:htmlcollection,    //readonly attribute HTMLCollection children;
  firstElementChild:element,    //readonly attribute Element? firstElementChild;
  lastElementChild:element,    //readonly attribute Element? lastElementChild;
  previousElementSibling:element,    //readonly attribute Element? previousElementSibling;
  nextElementSibling:element,    //readonly attribute Element? nextElementSibling;
  childElementCount:1    //readonly attribute unsigned long childElementCount;
}

var attr = {
  namespaceURI:"asfd",    //readonly attribute DOMString? namespaceURI;
  prefix:"Asf",    //readonly attribute DOMString? prefix;
  localName:"Asf",    //readonly attribute DOMString localName;
  name:"asf",    //readonly attribute DOMString name;
  value:"asdf"         //attribute DOMString value;
}

var documentType = {
  prototype:node,
  name:"asf",    //readonly attribute DOMString name;
  publicId:"asf",    //readonly attribute DOMString publicId;
  systemId:"Asf"    //readonly attribute DOMString systemId;
}

var processingInstruction = {
  prototype:node,
  target:"asdf",    //readonly attribute DOMString target;
  data:"sad"         //attribute DOMString data;
}

var characterData = {
  prototype:node,
  data:"sdf", //[TreatNullAs=EmptyString] attribute DOMString data;
  length:1,    //readonly attribute unsigned long length;
  substringData:function(a1, a2){return (a1 + a2).toString + this.data;}, //DOMString substringData(unsigned long offset, unsigned long count);
  appendData:function(a1){this.data = a1;}, //void appendData(DOMString data);
  insertData:function(a1, a2){this.data = a1 + a2;}, //void insertData(unsigned long offset, DOMString data);
  deleteData:function(a1, a2){this.data = a1 + a2;}, //void deleteData(unsigned long offset, unsigned long count);
  replaceData:function(a1, a2, a3){this.data = a3 + a1 + a2;} //void replaceData(unsigned long offset, unsigned long count, DOMString data);
}

var text = {
  prototype:characterData,
  splitTextt:function(a){if(a) return this.wholeText; else return this.wholeText;}, //Text splitText(unsigned long offset);
  wholeText:"asfd",    //readonly attribute DOMString wholeText;
  replaceWholeText:function(a){if(a) return this.wholeText; else return this.wholeText;} //Text? replaceWholeText(DOMString data);
}

var nodeList = {
  item:function(a){if(a) return node; else return node;}, //getter Node? item(unsigned long index);
  length:1    //readonly attribute unsigned long length;
}

var htmlCollection = {
  length:1,    //readonly attribute unsigned long length;
  item:function(a){if(a) return element; else return element;}, //getter Element? item(unsigned long index);
  namedItem:function(a){if(a) return element; else return element;} //getter Element? namedItem(DOMString name); // only returns Element
}
