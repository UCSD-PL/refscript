/// <reference path="../../../d3.d.ts" />

d3.keys = function(map:{ }): string[] 
/*@ <anonymous> (map: [Immutable]{ }) => MArray<{string | hasProperty(v, map) && enumProp(v, map)}> */
{
  var keys: string[] = [];
  for (var key in map) keys.push(key);
  return keys;
};
