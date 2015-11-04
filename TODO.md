Tests / Benchmarks
------------------

See https://github.com/UCSD-PL/rs-benchmarks


Tool/Implementation
-------------------

  - Better error message at 'ziptype l ...'

  - Disallow (buggy): module K.L.M, which can be replaced by

      module K { module L ... }


  - Mutability

      * What are good default mutabilities (parsing etc.)?

      * Checks on type parameters (including mutability - always first parameter)


  - Variables cannot be named: "func" or "obj" (fixpoint restriction)


  - WELL-FORMEDNESS CHECKS:

    * TRefs should have a mutability position

    * each sort should be represented at most once at a union

    * overloaded functions signatures are non-overlapping


  - Fix hacky qualifier parse-translation e.g. tests/liquid/pos/arrays/arr-03.js

          /* qualif OkLen(v:number, arr:a): v < (len arr) */

    Note use of lower-case which gets translated into tyvars in fixpoint. sigh.


  - Check polarity of type parameter in type


  - Array literal checks are quite slow.
      E.g.: typescript/pos/arrays/arr-07.js


----

env  [ x:{v : int | [(ttag([v]) = FIXPOINTSYMBOL_7); (Prop(v) <=> (v != 0))]}
      ; window:{FIXPOINTSYMBOL_46 : FAppTy (Window ) (Immutable ) | [extends_interface(FIXPOINTSYMBOL_46, FIXPOINTSYMBOL_47)
                                                                    ; Prop(FIXPOINTSYMBOL_46)
                                                                    ; (ttag([FIXPOINTSYMBOL_46]) = FIXPOINTSYMBOL_4)]}
      ; undefined:{v : Undefined  | [(ttag([v]) = FIXPOINTSYMBOL_2)
                                    ; (~ (Prop(v)))]}
      ; ttag:{VV : func(1, [@(0) ; Str ]) | []}
      ; r_SSA_4:{v : int | [(ttag([v]) = FIXPOINTSYMBOL_7)
                           ; (Prop(v) <=> (v != 0)); (v ~~ r_SSA_4)
                           ; (ttag([v]) = FIXPOINTSYMBOL_7)
                           ; (Prop(v) <=> (v != 0)); (v ~~ r_SSA_3)
                           ; (ttag([v]) = FIXPOINTSYMBOL_7)
                           ; (Prop(v) <=> (v != 0)); (v ~~ r_SSA_1)
                           ; (ttag([v]) = FIXPOINTSYMBOL_7)
                           ; (Prop(v) <=> (v != 0)); (v ~~ x)
                           ; (ttag([v]) = FIXPOINTSYMBOL_7)
                           ; (Prop(v) <=> (v != 0))]}
      ; r_SSA_3:{v : int | [(ttag([v]) = FIXPOINTSYMBOL_7)
                           ; (Prop(v) <=> (v != 0)); (v ~~ r_SSA_1)
                           ; (ttag([v]) = FIXPOINTSYMBOL_7)
                           ; (Prop(v) <=> (v != 0)); (v ~~ x)
                           ; (ttag([v]) = FIXPOINTSYMBOL_7)
                           ; (Prop(v) <=> (v != 0))]}
      ; r_SSA_1:{v : int | [(ttag([v]) = FIXPOINTSYMBOL_7)
                           ; (Prop(v) <=> (v != 0)); (v ~~ x)
                           ; (ttag([v]) = FIXPOINTSYMBOL_7)
                           ; (Prop(v) <=> (v != 0))]}
      ; r_SSA_0:{v : int | [(ttag([v]) = FIXPOINTSYMBOL_7)
                           ; (Prop(v) <=> (v != 0)); (v ~~ x)
                           ; (ttag([v]) = FIXPOINTSYMBOL_7)
                           ; (Prop(v) <=> (v != 0))]}
      ; offset:{VV : func(2, [@(0) ; Str  ; @(1)]) | []}
      ; numeric_positive_infinity:{VV : int | []}
      ; numeric_negative_infinity:{VV : int | []}
      ; numeric_nan:{VV : int | []}
      ; numeric_min_value:{VV : int | []}
      ; numeric_max_value:{VV : int | []}
      ; lq_tmp_nano_5:{v : Boolean  | [(ttag([v]) = FIXPOINTSYMBOL_49)
                                      ; (Prop(v) <=> Prop(lq_tmp_nano_2))]}
      ; lq_tmp_nano_2:{v : Boolean  | [(ttag([v]) = FIXPOINTSYMBOL_49)
                                      ; (Prop(v) <=> (x > lq_tmp_nano_1))]}
      ; lq_tmp_nano_1:{v : int | [(ttag([v]) = FIXPOINTSYMBOL_7)
                                 ; (Prop(v) <=> (v != 0)); (v ~~ 0)]}
      ; len:{VV : func(2, [FAppTy (FAppTy (Array ) @(0)) @(1) ; int]) | []}
      ; hasProperty:{VV : func(1, [Str  ; @(0) ; bool]) | []}
      ; hasDirectProperty:{VV : func(1, [Str  ; @(0) ; bool]) | []}
      ; extends_interface:{VV : func(1, [@(0) ; Str  ; bool]) | []}
      ; extends_class:{VV : func(1, [@(0) ; Str  ; bool]) | []}
      ; enumProp:{VV : func(1, [Str  ; @(0) ; bool]) | []}
      ; document.documentElement:{VV : FAppTy (HTMLElement ) (Immutable ) |
                                 [extends_interface(VV, FIXPOINTSYMBOL_44)
                                 ; Prop(VV); (ttag([VV]) = FIXPOINTSYMBOL_4)
                                 ; (VV ~~ offset([document; FIXPOINTSYMBOL_45]))]}
      ; document:{FIXPOINTSYMBOL_42 : FAppTy (Document ) (Immutable ) |
                 [extends_interface(FIXPOINTSYMBOL_42, FIXPOINTSYMBOL_43)
                 ; Prop(FIXPOINTSYMBOL_42)
                 ; (ttag([FIXPOINTSYMBOL_42]) = FIXPOINTSYMBOL_4)]}
      ; console:{FIXPOINTSYMBOL_34 : FAppTy (Console ) (Immutable ) |
                [extends_interface(FIXPOINTSYMBOL_34, FIXPOINTSYMBOL_33)
                ; Prop(FIXPOINTSYMBOL_34)
                ; (ttag([FIXPOINTSYMBOL_34]) = FIXPOINTSYMBOL_4)]}
      ; bvor:{VV : func(1, [FAppTy (BitVec ) @(0) ; FAppTy (BitVec ) @(0) ; FAppTy (BitVec ) @(0)]) | []}
      ; bvand:{VV : func(1, [FAppTy (BitVec ) @(0) ; FAppTy (BitVec ) @(0) ; FAppTy (BitVec ) @(0)]) | []}
      ; arguments.length:{v : int | [(ttag([v]) = FIXPOINTSYMBOL_7)
                                    ; (Prop(v) <=> (v != 0))
                                    ; (v ~~ offset([arguments; FIXPOINTSYMBOL_48]))
                                    ; (v ~~ 1)]}
      ; arguments:{VV : Object  | [Prop(VV); (ttag([VV]) = FIXPOINTSYMBOL_4)]}
      ; String.prototype:{VV : FAppTy (String ) (Immutable ) | [extends_interface(VV, FIXPOINTSYMBOL_27)
                                                               ; Prop(VV)
                                                               ; (ttag([VV]) = FIXPOINTSYMBOL_4)
                                                               ; (VV ~~ offset([String; FIXPOINTSYMBOL_6]))]}
      ; String:{FIXPOINTSYMBOL_25 : FAppTy (StringConstructor ) (Immutable ) |
               [extends_interface(FIXPOINTSYMBOL_25, FIXPOINTSYMBOL_26)
               ; Prop(FIXPOINTSYMBOL_25)
               ; (ttag([FIXPOINTSYMBOL_25]) = FIXPOINTSYMBOL_4)]}
      ; Set_sub:{VV : func(1, [FAppTy (Set_Set ) @(0) ; FAppTy (Set_Set ) @(0) ; bool]) | []}
      ; Set_sng:{VV : func(1, [@(0) ; FAppTy (Set_Set ) @(0)]) | []}
      ; Set_mem:{VV : func(1, [@(0) ; FAppTy (Set_Set ) @(0) ; bool]) | []}
      ; Set_empty:{VV : func(1, [int ; FAppTy (Set_Set ) @(0)]) | []}
      ; Set_emp:{VV : func(1, [FAppTy (Set_Set ) @(0) ; bool]) | []}
      ; Set_dif:{VV : func(1, [FAppTy (Set_Set ) @(0) ; FAppTy (Set_Set ) @(0) ; FAppTy (Set_Set ) @(0)]) | []}
      ; Set_cup:{VV : func(1, [FAppTy (Set_Set ) @(0) ; FAppTy (Set_Set ) @(0) ; FAppTy (Set_Set ) @(0)]) | []}
      ; Set_cap:{VV : func(1, [FAppTy (Set_Set ) @(0) ; FAppTy (Set_Set ) @(0) ; FAppTy (Set_Set ) @(0)]) | []}
      ; Prop:{VV : func(1, [@(0) ; bool]) | []}
      ; Object.prototype:{VV : FAppTy (Object ) (Immutable ) | [extends_interface(VV, FIXPOINTSYMBOL_5)
                                                               ; Prop(VV)
                                                               ; (ttag([VV]) = FIXPOINTSYMBOL_4)
                                                               ; (VV ~~ offset([Object; FIXPOINTSYMBOL_6]))]}
      ; Object:{FIXPOINTSYMBOL_3 : Object  | [Prop(FIXPOINTSYMBOL_3)
                                             ; (ttag([FIXPOINTSYMBOL_3]) = FIXPOINTSYMBOL_4)]}
      ; Number.prototype:{VV : FAppTy (Number ) (Immutable ) | [extends_interface(VV, FIXPOINTSYMBOL_11)
                                                               ; Prop(VV)
                                                               ; (ttag([VV]) = FIXPOINTSYMBOL_4)
                                                               ; (VV ~~ offset([Number; FIXPOINTSYMBOL_6]))]}
      ; Number.POSITIVE_INFINITY:{v : int | [(ttag([v]) = FIXPOINTSYMBOL_7)
                                            ; (Prop(v) <=> (v != 0))
                                            ; (v ~~ offset([Number; FIXPOINTSYMBOL_9]))]}
      ; Number.NaN:{v : int | [(ttag([v]) = FIXPOINTSYMBOL_7)
                              ; (Prop(v) <=> (v != 0))
                              ; (v ~~ offset([Number; FIXPOINTSYMBOL_12]))]}
      ; Number.NEGATIVE_INFINITY:{v : int | [(ttag([v]) = FIXPOINTSYMBOL_7)
                                            ; (Prop(v) <=> (v != 0))
                                            ; (v ~~ offset([Number; FIXPOINTSYMBOL_13]))]}
      ; Number.MIN_VALUE:{v : int | [(ttag([v]) = FIXPOINTSYMBOL_7)
                                    ; (Prop(v) <=> (v != 0))
                                    ; (v ~~ offset([Number; FIXPOINTSYMBOL_10]))]}
      ; Number.MAX_VALUE:{v : int | [(ttag([v]) = FIXPOINTSYMBOL_7)
                                    ; (Prop(v) <=> (v != 0))
                                    ; (v ~~ offset([Number; FIXPOINTSYMBOL_14]))]}
      ; Number:{FIXPOINTSYMBOL_8 : Object  | [Prop(FIXPOINTSYMBOL_8)
                                             ; (ttag([FIXPOINTSYMBOL_8]) = FIXPOINTSYMBOL_4)]}
      ; NaN:{v : int | [(ttag([v]) = FIXPOINTSYMBOL_7)
                       ; (Prop(v) <=> (v != 0)); (v = numeric_nan)]}
      ; Math.SQRT2:{v : int | [(ttag([v]) = FIXPOINTSYMBOL_7)
                              ; (Prop(v) <=> (v != 0))
                              ; (v ~~ offset([Math; FIXPOINTSYMBOL_17]))]}
      ; Math.SQRT1_2:{v : int | [(ttag([v]) = FIXPOINTSYMBOL_7)
                                ; (Prop(v) <=> (v != 0))
                                ; (v ~~ offset([Math; FIXPOINTSYMBOL_23]))]}
      ; Math.PI:{v : int | [(ttag([v]) = FIXPOINTSYMBOL_7)
                           ; (Prop(v) <=> (v != 0))
                           ; (v ~~ offset([Math; FIXPOINTSYMBOL_19]))]}
      ; Math.LOG2E:{v : int | [(ttag([v]) = FIXPOINTSYMBOL_7)
                              ; (Prop(v) <=> (v != 0))
                              ; (v ~~ offset([Math; FIXPOINTSYMBOL_21]))]}
      ; Math.LOG10E:{v : int | [(ttag([v]) = FIXPOINTSYMBOL_7)
                               ; (Prop(v) <=> (v != 0))
                               ; (v ~~ offset([Math; FIXPOINTSYMBOL_20]))]}
      ; Math.LN2:{v : int | [(ttag([v]) = FIXPOINTSYMBOL_7)
                            ; (Prop(v) <=> (v != 0))
                            ; (v ~~ offset([Math; FIXPOINTSYMBOL_18]))]}
      ; Math.LN10:{v : int | [(ttag([v]) = FIXPOINTSYMBOL_7)
                             ; (Prop(v) <=> (v != 0))
                             ; (v ~~ offset([Math; FIXPOINTSYMBOL_24]))]}
      ; Math.E:{v : int | [(ttag([v]) = FIXPOINTSYMBOL_7)
                          ; (Prop(v) <=> (v != 0))
                          ; (v ~~ offset([Math; FIXPOINTSYMBOL_22]))]}
      ; Math:{FIXPOINTSYMBOL_15 : FAppTy (Math ) (Immutable ) | [extends_interface(FIXPOINTSYMBOL_15, FIXPOINTSYMBOL_16)
                                                                ; Prop(FIXPOINTSYMBOL_15)
                                                                ; (ttag([FIXPOINTSYMBOL_15]) = FIXPOINTSYMBOL_4)]}
      ; Map_store:{VV : func(2, [FAppTy (FAppTy (Map_t ) @(0)) @(1) ; @(0) ; @(1) ; FAppTy (FAppTy (Map_t ) @(0)) @(1)]) | []}
      ; Map_select:{VV : func(2, [FAppTy (FAppTy (Map_t ) @(0)) @(1) ; @(0) ; @(1)]) | []}
      ; Function.prototype:{VV : FAppTy (Function ) (Immutable ) | [extends_interface(VV, FIXPOINTSYMBOL_31)
                                                                   ; Prop(VV)
                                                                   ; (ttag([VV]) = FIXPOINTSYMBOL_4)
                                                                   ; (VV ~~ offset([Function; FIXPOINTSYMBOL_6]))]}
      ; Function:{FIXPOINTSYMBOL_30 : Object  | [Prop(FIXPOINTSYMBOL_30)
                                                ; (ttag([FIXPOINTSYMBOL_30]) = FIXPOINTSYMBOL_4)]}
      ; Event.prototype:{VV : FAppTy (Event ) (Immutable ) | [extends_interface(VV, FIXPOINTSYMBOL_40)
                                                             ; Prop(VV)
                                                             ; (ttag([VV]) = FIXPOINTSYMBOL_4)
                                                             ; (VV ~~ offset([Event; FIXPOINTSYMBOL_6]))]}
      ; Event.CAPTURING_PHASE:{v : int | [(ttag([v]) = FIXPOINTSYMBOL_7)
                                         ; (Prop(v) <=> (v != 0))
                                         ; (v ~~ offset([Event; FIXPOINTSYMBOL_38]))]}
      ; Event.BUBBLING_PHASE:{v : int | [(ttag([v]) = FIXPOINTSYMBOL_7)
                                        ; (Prop(v) <=> (v != 0))
                                        ; (v ~~ offset([Event; FIXPOINTSYMBOL_41]))]}
      ; Event.AT_TARGET:{v : int | [(ttag([v]) = FIXPOINTSYMBOL_7)
                                   ; (Prop(v) <=> (v != 0))
                                   ; (v ~~ offset([Event; FIXPOINTSYMBOL_39]))]}
      ; Event:{FIXPOINTSYMBOL_37 : Object  | [Prop(FIXPOINTSYMBOL_37)
                                             ; (ttag([FIXPOINTSYMBOL_37]) = FIXPOINTSYMBOL_4)]}
      ; Error.prototype:{VV : FAppTy (Error ) (Immutable ) | [extends_interface(VV, FIXPOINTSYMBOL_36)
                                                             ; Prop(VV)
                                                             ; (ttag([VV]) = FIXPOINTSYMBOL_4)
                                                             ; (VV ~~ offset([Error; FIXPOINTSYMBOL_6]))]}
      ; Error:{FIXPOINTSYMBOL_35 : Object  | [Prop(FIXPOINTSYMBOL_35)
                                             ; (ttag([FIXPOINTSYMBOL_35]) = FIXPOINTSYMBOL_4)]}
      ; Console.prototype:{VV : FAppTy (Console ) (Immutable ) | [extends_interface(VV, FIXPOINTSYMBOL_33)
                                                                 ; Prop(VV)
                                                                 ; (ttag([VV]) = FIXPOINTSYMBOL_4)
                                                                 ; (VV ~~ offset([Console; FIXPOINTSYMBOL_6]))]}
      ; Console:{FIXPOINTSYMBOL_32 : Object  | [Prop(FIXPOINTSYMBOL_32)
                                               ; (ttag([FIXPOINTSYMBOL_32]) = FIXPOINTSYMBOL_4)]}
      ; Array.prototype:{VV : FAppTy (FAppTy (Array ) (Mutable )) (Top ) |
                        [extends_interface(VV, FIXPOINTSYMBOL_29); Prop(VV)
                        ; (ttag([VV]) = FIXPOINTSYMBOL_4)
                        ; (VV ~~ offset([Array; FIXPOINTSYMBOL_6]))]}
      ; Array:{FIXPOINTSYMBOL_28 : Object  | [Prop(FIXPOINTSYMBOL_28)
                                             ; (ttag([FIXPOINTSYMBOL_28]) = FIXPOINTSYMBOL_4)]}]
 lhs {FIXPOINTSYMBOL_55 : int | [(ttag([FIXPOINTSYMBOL_55]) = FIXPOINTSYMBOL_7)
                                ; (Prop(FIXPOINTSYMBOL_55) <=> (FIXPOINTSYMBOL_55 != 0))
                                ; (FIXPOINTSYMBOL_55 ~~ r_SSA_4)
                                ; (ttag([FIXPOINTSYMBOL_55]) = FIXPOINTSYMBOL_7)
                                ; (Prop(FIXPOINTSYMBOL_55) <=> (FIXPOINTSYMBOL_55 != 0))
                                ; (FIXPOINTSYMBOL_55 ~~ r_SSA_4)
                                ; (ttag([FIXPOINTSYMBOL_55]) = FIXPOINTSYMBOL_7)
                                ; (Prop(FIXPOINTSYMBOL_55) <=> (FIXPOINTSYMBOL_55 != 0))
                                ; (FIXPOINTSYMBOL_55 ~~ r_SSA_3)
                                ; (ttag([FIXPOINTSYMBOL_55]) = FIXPOINTSYMBOL_7)
                                ; (Prop(FIXPOINTSYMBOL_55) <=> (FIXPOINTSYMBOL_55 != 0))
                                ; (FIXPOINTSYMBOL_55 ~~ r_SSA_1)
                                ; (ttag([FIXPOINTSYMBOL_55]) = FIXPOINTSYMBOL_7)
                                ; (Prop(FIXPOINTSYMBOL_55) <=> (FIXPOINTSYMBOL_55 != 0))
                                ; (FIXPOINTSYMBOL_55 ~~ x)
                                ; (ttag([FIXPOINTSYMBOL_55]) = FIXPOINTSYMBOL_7)
                                ; (Prop(FIXPOINTSYMBOL_55) <=> (FIXPOINTSYMBOL_55 != 0))
                                ; Prop(lq_tmp_nano_5)]}
 rhs {FIXPOINTSYMBOL_55 : int | [k__9[FIXPOINTSYMBOL_53:=FIXPOINTSYMBOL_55][FIXPOINTSYMBOL_50:=FIXPOINTSYMBOL_55]]}
 id 4 tag [1] //

Fixpoint: Bad Constraint! id = 4 (FixConstraint.BadConstraint(_)) tag = tag [1] //
