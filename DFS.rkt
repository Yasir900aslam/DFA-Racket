;;Completed on 12:50 . 12/1/2017
;;This program has a dependency of dyoo-while-loop .
;;copy paste below command in Racket shell
;; raco pkg install while-loop
;;Then This program will run , I have tested it with many machine and final states and the results so far are positive , and it test multiple strings .

;;Here if you entered Number of states is 3 then your states name will be
;; q0 , q1 , q2 here q0 is the start state
;;Then select final state from any one of these

;;START OF REQUIRE OR GLOBALS
#lang racket
(require ffi/unsafe)
(require dyoo-while-loop) 
;below code converts the user inputs to 2d list then flatten's it
;Flatten the list L
(define (flatten L)
  (cond
    [(list? L)
     (apply append(map flatten L))]
    [else (list L)]))
(define user-string "")
(define in 0)
(define state 0)
;Statt of dfa globals
(define x null)
(define nStates null)
(define nInputs 2)
(define inputVar (list #\a #\b))

;end of dfa globals
;;START OF FINAL STATES ARRAY Functions and Globals
(define nFinals 0)
(define t1 null)
(define x1 null)
(define a1 null);a1 will be pointing to array of final states so use this
;Setter and getter of final states
(define (setfinal index value)
  (array-set! a1 index value)
  )
(define (getfinal index)
  (array-ref a1 index)
  )
;;End of setter and getters
(define(final-states)
(printf "\nEnter number of final states: ") 
(set! nFinals (read))
 (create-array)
(for ([p nFinals])
  (define new p)
  (printf"Final State ~a:q" (+ 1 new))
  (define new1 0)
  (set! new1 (read))
  (setfinal p new1) ;Initialize the array here
  )
  null
  )

;;END OF FINAL STATES ARRAY AND GLOBALS

;START OF 2D ARRAYS DEFINITIONS
(define t2d null)
(define x2d null)
(define a2d null)
;END OF 2D ARRAY DEFINATION

;;START OF CTYPE ARRAYS 2D FOR MACHINE STATES
(define (create-array2D)
       (set! t2d (_array _int nStates 2))
       (set! x2d (malloc t2d))
       (set! a2d (ptr-ref x2d t2d 0))       
       )
(define (create-array)
       (set! t1 (_array _int nFinals))
       (set! x1 (malloc t1))
       (set! a1 (ptr-ref x1 t1 0))
  )
;;END OF CTYPE ARRAYS 2D FOR MACHINE STATES


;;START OF DFA-Inputs 
(define (start)
  (printf "Enter the number of States in your DFA(including start state q0): ")
  (set! nStates (read))
  (printf "Total Number of inputs are : ")
  (display nInputs)
  (printf "\nYour Input Variable are ~a and ~a" (first inputVar) (list-ref inputVar 1))
  )
(define (dfa-sim )
  (printf "|------------------------------------------------------------------------\n")
  (printf "|define transition rule as (initial state, input symbol ) = final state\n")
  (printf "|------------------------------------------------------------------------\n")
  (for* ([i nStates] [j nInputs])
  (printf "(q~a"i)
  (printf ",")
  (define n (list-ref inputVar j))
  (printf "~a" n)
  (printf "=q:")
  (set! x (read))
  (printf "i~a j~a\n"i j)
  (array-set! a2d i j x)
  (display (array-ref a2d i j))
  )
  )
;;END OF DFA

;;END OF REQUIRE OR GLOBALS


;--------------------------------------------------------------------------------



;;START OF MACHINE CREATION STATE

 (start) 
 (create-array2D) ;create the 2d array or the transition table
 (final-states) ;get final states and put them in a list
 (dfa-sim ) ; initialize the 2d arrays with values by user

;;End OF MACHINE CREATION STATE

;----------------------------------------------------------------------------------

;;Some function for usage below
;;below converts list into string bease (read) return a list so we convert it to list so comparing purposes
(define (slist slst)
  (cond ((empty? slst) "")
        ((empty? (rest slst)) (symbol->string (first slst)))
        (else (string-append (symbol->string (first slst))
                             " "
                             (slist (rest slst))))))
(define m null)
(define(check-return a b)
   (array-ref a2d a b))


(define j 0)
(define a 0)

;The Check function.
(define (check charb inta);charb=character of string and inta =state
  (while (< j 2)
    (set! m (list-ref inputVar j))
    (set! a (+ a 1))
    (if (equal? charb (list-ref inputVar j))
        (set! state (array-ref a2d inta j))
        (break))
    (set! j (+ j 1))
    )
  (set! j 0)
    )


;modified read-line.
(define (my-read-line)
  (let ([contents (read-line)])
    (if (string=? contents "")
      (read-line)
      contents)))


;;end of these function or globals
(define qp 0)
(define np null)
(define ii 0)
(define final 0)
(define new null)
;;START OF USER GENERATED STRING TO BE TESTED
(while #t
   (printf "\n\n\nWhen you inserting Test String Do not Put Space Betwwen Character as They will be voided by text buffer\n")
   (printf "Enter String: ")
   (set! user-string (my-read-line))
   (display user-string)
   (define length (string-length user-string))
  (while (< ii length)
    (set! new (string-ref user-string ii))
    (set! ii (+ ii 1))
    (check new state)
     (cond [(void? state)
            (set! state 0)])
     )
(for ([i (in-range nFinals)])
   (set! np (getfinal i))
         (cond [(equal? np state)
                (set! final 1)]))
  (if(equal? 1 final)
     (printf"\naccepted\n")
      (printf "\nrejected\n"))
  (printf "Enter more Strings y/n: ")
  (define xx (read))
  (define n 'n)
  (set! state 0)
  (set! final 0)
  (set! ii 0)
  (when (equal? n xx)
    (break)))

;;END OF USER GENERATED STRING TO BE TESTED


