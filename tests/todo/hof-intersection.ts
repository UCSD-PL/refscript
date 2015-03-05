//adapted from d3

/*@ d3_bisector :: forall T . ((T)=>number) => void */
declare function d3_bisector(comp);

/*@ d3_ascending :: /\ (number) => {number | v > 0}
                    /\ forall T . (T) => number */
declare function d3_ascending(a);

/*@ d3_ascending_1 :: (number) => {number | v > 0} */
var d3_ascending_1 = d3_ascending;

var d3_bisect = d3_bisector(d3_ascending);

