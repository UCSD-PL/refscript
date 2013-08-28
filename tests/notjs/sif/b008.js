

function handler(e) {
  e.preventBubble();
  e.preventDefault();
  e.stopPropagation();
  // the above calls should literally do nothing
  PROBE(e.bubbles);
}

document.addEventListener("keypress", handler, false);


