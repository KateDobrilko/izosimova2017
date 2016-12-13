#lang racket
(require (lib "defmacro.ss"))
(define subList1 (list 0 (list 1) 2 3 "Lisa"))
(define subList2 (list 4 5  #\k (list 1) '()))
(define pair (cons "numberToLabel" (cons 1 "First")))
(define num 5)
(define str "Kate")
(define sym 'a)
(define char #\d)
(define emptyList '())
(define multiValueList (list  subList1  pair num str sym char subList2 emptyList))
(define resultList '())
(define globalResult'())
(define-syntax-rule (getAtomsInListsOfList listV)
( let ((mas listV))
(define addElementsOfSublistToGlobalList(lambda(subList)
  (cond((not (null? subList))(cond ((not (null? (car subList)))
            (set! globalResult (cons(car subList)  globalResult ))(addElementsOfSublistToGlobalList (cdr subList))
                  ))))))

(define findAtomsInSublist (lambda (subL)
  (cond((not (null? subL))(cond((pair? (car subL))(findAtomsInSublist (cdr subL)))
      (else (cond((not(and(null? (car subL))))
            (set! resultList (cons(car subL) resultList)) (findAtomsInSublist (cdr subL)))
                  )))))))

(define getListOfAtomsInSubLists(lambda(listVal)(set! resultList '())
  (cond((null? listVal)(getListOfAtomsInSubLists(cdr listVal)))
     (else(cond ((not(null? (car listVal)))(           
            cond((list?(car listVal))(findAtomsInSublist (car listVal))(
               addElementsOfSublistToGlobalList resultList)
              (getListOfAtomsInSubLists(cdr listVal)))
              (else(getListOfAtomsInSubLists(cdr listVal))))            
  )
)))))

(getListOfAtomsInSubLists mas)))

(getAtomsInListsOfList multiValueList)
(display globalResult)


