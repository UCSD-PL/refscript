/*: dispatch :: [A,B] (x:Ref, f:Str)
    / (x: {(and (= (tag v) "Dict") (v :: A)
           ((sel v f) :: (Ref(x)) / (x: A > x.pro) -> B / same))} > x.pro)
   -> B / same */ "#type";
var dispatch = function(x,f) {
  var fn = x[f];
  return fn(x);
};


dispatch(
      {a: function f(x) { 
            var s = 0;
            for (i in x) {
              if (typeof x[i] == 'number')              
                s += x[i];
            }
            return s;
          },
      b: 1,
      c: 2,
      d: 3
      }, "a");
