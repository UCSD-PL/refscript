
var a = { f1: 2 };

a.f1 = 1;


/*@ babis :: [#Mutable]{ f1: { number | v > 1 }; f3: boolean } */
var babis = { f1: 2, f3: true };

babis.f3 = false;

