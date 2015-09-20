
/*@ type idx<x> = { v: number | [0 <= v; v < len(x)] } */
declare type idx = number;

/*@ type posint = { v: number | 0 < v } */
declare type posint = number;

/*@ type negint = { v: number | v < 0 } */
declare type negint = number;
