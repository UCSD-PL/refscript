/*@ qualif Bot(v:a, s:string): hasProperty(v,s) */

/*@ predicate InstIterator(V) = (hasProperty("next", V) => extends_interface(V,"Iterator")) */

// Just happes to be already defined in prelude.ts
// interface EmptyObject { }

/*@ alias ObjectK = { v: EmptyObject<Immutable> | InstIterator(v) } */

interface Iterator extends EmptyObject { next(): number; }

/*@ isIterable :: forall T . (x: ObjectK) => { boolean | (Prop v) => (extends_interface(x,"Iterator")) } */
function isIterable(x) { 
    return "next" in x;
}

/*@ reduce :: (ObjectK) => {void | true} */
function reduce(coll) {
    if(isIterable(coll)) {
        /*@ z :: Iterator<Immutable> */
        /* z :: {[Immutable]{ } | extends_interface(v, "Iterator")} */
        var z = <Iterator>coll;
    }
}
