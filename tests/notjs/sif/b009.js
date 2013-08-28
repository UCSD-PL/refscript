

function handler(e) {
  var ele = e.target.getElementById("foo");
  PROBE(ele);
}

document.addEventListener("keypress", handler, false);

