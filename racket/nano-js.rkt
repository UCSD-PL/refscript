#lang racket

(require redex)

(define-language L
  (B int
     bool
     (Array B)
  )
  
  (T (RefT B p)
     (→ (x T) ... T)
  )
  
  (E 
     x
     c
     (Call E E ...) 
  )
  
  (S skip
     (Ass x E)
     (Seq S S)
     (If E E E)
     (Return E)
  )
  
  (F (Function c x ... S))
  
  (AF (AnnotFunction F T))
  
  (P (Program AF ... ))
  
  (x variable-not-otherwise-mentioned)
)


(with-compound-rewriters
           (['Array 
             (λ (lws) (list "[" (list-ref lws 2) "]"))]
            
            ['B 
             (λ (lws) (list "" (list-ref lws 2)))]
            
            ['→
             (λ (lws) (list "" (list-ref lws 2) (list-ref lws 3) " → " (list-ref lws 4) ))]
            
            ['Call
             (λ (lws) (list "" (list-ref lws 2) "(" (list-ref lws 3) ")" ))]
            
            ['Ass
             (λ (lws) (list "" (list-ref lws 2) ":=" (list-ref lws 3)))]
            
            ['Seq
             (λ (lws) (list "" (list-ref lws 2) "; " (list-ref lws 3)))]
            
            ['If
             (λ (lws) (list "if " (list-ref lws 2) " then " (list-ref lws 3) " else " (list-ref lws 4)))]
            
            ['Return
             (λ (lws) (list "return " (list-ref lws 2)))]
            
            ['Function
             (λ (lws) (list "function " (list-ref lws 2) " (" (list-ref lws 3) "... )" (list-ref lws 5) ))]
            
            ['AnnotFunction
             (λ (lws) (list "" (list-ref lws 2) " :: " (list-ref lws 3)))]
                        
            ['Program
             (λ (lws) (list "" (list-ref lws 2) (list-ref lws 3)))]
            )
           
           (render-language L))