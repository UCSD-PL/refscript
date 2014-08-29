JQuery Sizes as Refinements
===========================

i.e. porting http://cs.brown.edu/research/plt/dl/jquery/jquery.pdf

measure size :: (Type, JQuery) -> Nat

Translation
-----------

    tx(m1 ++ m2) = tx(m1) && tx(m2)
    tx(m<T>)     = tx(tx(T), m)

    tx(T)        = size(T, v)
    tx(x, i)     = x =  i
    tx(x,i+)     = x >= i
    tx(x,ij)     = x = i || x = j

Encoding Tree Shape
-------------------

Set of rules

  Ti -> Ti_1?...Ti_n?

Such that each T appears **at most once** on each LHS/RHS

Crunch down into a set of relations:

  par : (T, T) -> Bool
  sib : (T, T) -> Bool


JQuery Specifications
---------------------

    this.parent()   ::  {v: JQuery | /\_par(T,T') size(T, v) = size(T', this) }

    this.child()    ::  {v: JQuery | /\_par(T',T) size(T, v) = size(T', this) }

    this.next()     ::  {v: JQuery | /\_sib(T,T') size(T, v) = size(T', this) }

    this.prev()     ::  {v: JQuery | /\_sib(T',T) size(T, v) = size(T', this) }

    this.add(that)  ::  {v: JQuery | /\_T         size(T, v) = size(T, this) + size(T, that) }
    
    this.filter(c)  ::  {v: JQuery | /\_T         size(T, v) = @match(T, c) ? size(T, this) : 0 }

