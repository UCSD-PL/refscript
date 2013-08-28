/*: (~lPass: {Dict|(implies (has v "weight") (Num (sel v "weight")))} > lObjPro) */ "#weak";

/*: sumWeight ::
      (passengers:Ref, max_weight:Num)
    / (passengers: {Arr(Ref(~lPass))|(packed v)} > lArrPro) -> Num / same */ "#type";

var sumWeight = function(passengers, max_weight) {
  var sum /*: Num */ = 0.0, n = passengers.length;
  for (var i /*: {Int|(>= v 0)} */ = 0; i < n; i++) {
    var p = passengers[i];
    /*: p lThaw1 */ "#thaw";
    if (p.weight) { sum += p.weight;   }
    else          { sum += max_weight; }
    /*: p (~lPass, thwd lThaw1) */ "#freeze";
  }
  return sum;
};


sumWeight(
  [ {weight : 10, a: "a"},
    {weight : 10.0, a: "a"},
    {weight : 10.1, a: "a"},
    {weight : 0},
    {weight : 10, a: "a"},
    {weight : 10, a: "a"},
    {weight : 10, a: "a"} ], 10);     //notjs can't figure it out
