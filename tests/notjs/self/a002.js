var obj = {
	f: function(x) {if (x != 0) return this.g(0); else return 23;},
	g: function(x) {if (x != 0) return this.f(0); else return 23;}
};

var g = function(x) {return 45;};
var f = function(x) {return 45;};
var z = obj.f;

z(4) + obj.f(4); // Must return  (23 + 45) = 68


/* NOtJS 
(window#"obj":=({"f":([self,x]->( if (x = 0.0) then 1 else (self."f"(0.0)) ))})); window."obj"."f"(4)
*/
