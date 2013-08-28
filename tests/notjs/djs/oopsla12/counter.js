var toNum = function(x) /*: (Top) -> Num */ {
  if (typeof(x) == "number")       { return x; }
  else if (typeof(x) == "boolean") { return x ? 1 : 0; }
  else                             { return -1; }
};

var getCount = function(t,c) /*: (t:Ref, Str) / (t: Dict > t.pro) -> Num / same */ {
  if (c in t) {
    return toNum(t[c]);
  } else {
    return 0;
  }
};

/*: incCount :: (t:Ref, c:Str) / (t: dt:Dict > t.pro)
             -> Top / (t: {(and (eqmod v dt {c}) (Num (sel v c)))} > t.pro) */ "#type";
var incCount = function(t,c) {
  var i = getCount(t,c);
  t[c] = 1 + i;
};

var incThree = function(t,c) {
  incCount(t,c);
  incCount(t,c);
  incCount(t,c);
};


function recCount(t, c, n) {
  if (n > 0) {
    incCount(t,c);
    recCount(t,c,n-1);
  }
  return;
}


var d = { 
  a: 0,
  b: 0,
  c: "a",
  d: 0
};

var i = 0
for (; i< 100; i ++) {
  incCount(d, "a");
}

incThree(d, "b");
incCount(d, "c");
recCount(d, "d", 10);
//d.a;    //notjs cannot figure out
//d.b;    //3-2-stack_cfa
//d.c;    //3-2-stack_cfa
d.d;      //notjs cannot figure out
