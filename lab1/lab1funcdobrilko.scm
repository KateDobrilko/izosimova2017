(define subList1 (list 0 (list 1) 2 3 "Lisa"))
(define subList2 (list 4 5  #\k (list 1) ()))
(define pair (cons "numberToLabel" (cons 1 "First")))
(define num 5)
(define str "Kate")
(define sym 'a)
(define char #\d)
(define emptyList ())
(define multiValueList (list  subList1  pair num str sym char subList2 emptyList))
(define resultList ())
(define globalResult())

(define (addElementsOfSublistToGlobalList subList)
  (if(null? subList)()(begin( begin(if(null? (car subList))()
            (begin(set! globalResult (cons(car subList)  globalResult ))(addElementsOfSublistToGlobalList (cdr subList))
                  ))))))

(define (getListOfAtomsInSubLists listVal)(set! resultList ())
  (if(null? listVal)(getListOfAtomsInSubLists(cdr listVal))
     (begin(if(null? (car listVal))()(begin(            
            if(list?(car listVal))(begin(findAtomsInSublist (car listVal))(
               addElementsOfSublistToGlobalList resultList)
              (getListOfAtomsInSubLists(cdr listVal)))
              (getListOfAtomsInSubLists(cdr listVal)))            
  ))
)))

(define (findAtomsInSublist subList)
  (if(null? subList)()(begin( if(pair? (car subList))(findAtomsInSublist (cdr subList))
      (begin(if(and(null? (car subList)))()
            (begin(set! resultList (cons(car subList) resultList))(findAtomsInSublist (cdr subList))
                  )))))))

(getListOfAtomsInSubLists multiValueList)
(display globalResult)