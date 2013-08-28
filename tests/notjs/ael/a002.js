var x = 1;
function foo() { x = "str"; x }
function bar() { x = true; x }

window.addEventListener("click", foo);
window.addEventListener("mouseover", bar);
x
