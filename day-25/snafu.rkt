#lang racket


; First, practice converting SNAFU numbers to integers.

; Technically, you don't need to do this to solve the puzzle,
; as you can do the arithmetic entirely in base SNAFU.

; But, it's handy to have this while debugging your SNAFU arithmetic

; Converts a snafu number directly to an integer 
(define (snafu->integer snafu-number)

  (define (snafu-char->integer char)
    (match char
      [#\2  2]
      [#\1  1]
      [#\0  0]
      [#\-  -1]
      [#\=  -2]))

  (define (snafu-reverse-char-list->integer chars)
    (match chars
      [(list digit)    (snafu-char->integer digit)]
      [(cons hd tl)    (+ (* 5 (snafu-reverse-char-list->integer tl)) (snafu-char->integer hd))]))
 
  (snafu-reverse-char-list->integer (reverse (string->list snafu-number))))
    

;; Base SNAFU arithmetic

; For doing base SNAFU arithmetic, we'll represent SNAFU number as reversed lists of characters.

; For instance, SNAFU number "10=" will be represented as '(#\= #\0 #\1)


; Next, we can define the base case for SNAFU arithmetic, of single SNAFU digits that
; could end up with a carry into the next digit:
 
(define (+/snafu-char char1 char2)
  (match (list char1 char2)
   [(list #\0 a)      (list a)]           ; 0 + a = 0
   [(list a #\0)      (list a)]           ; a + 0 = a

   [(list #\1 #\1)    (list #\2)]         ;  1 +  1 = 2
   [(list #\- #\-)    (list #\=)]         ; -1 + -1 = -2

   [(list #\- #\1)    (list #\0)]         ; -1 +  1 = 0
   [(list #\1 #\-)    (list #\0)]         ;  1 + -1 = 0

   [(list #\2 #\=)    (list #\0)]         ;  2 + -2 = 0
   [(list #\= #\2)    (list #\0)]         ; -2 +  2 = 0

   [(list #\2 #\-)    (list #\1)]         ;  2 + -1 = 1
   [(list #\- #\2)    (list #\1)]         ; -1 +  2 = 1

   [(list #\= #\1)    (list #\-)]         ; -2 +  1 = -1
   [(list #\1 #\=)    (list #\-)]         ;  1 + -2 = -1

   [(list #\2 #\1)    (list #\= #\1)]     ;  2 +  1 = -2 + 5
   [(list #\1 #\2)    (list #\= #\1)]     ;  1 +  2 = -2 + 5

   [(list #\= #\-)    (list #\2 #\-)]     ; -2 + -1 =  2 + -5
   [(list #\- #\=)    (list #\2 #\-)]     ; -1 + -2 =  2 + -5

   [(list #\2 #\2)    (list #\- #\1)]     ;  2 +  2 = -1 +  5

   [(list #\= #\=)    (list #\1 #\-)]))   ; -2 + -2 =  1 + -5


; Sums two snafu nubers in revers char list form:
(define (+/snafu-reverse-char-list snafu-rev-chars-1 snafu-rev-chars-2)
  (match (list snafu-rev-chars-1 snafu-rev-chars-2)
    
    [(list a '())   a] 
    [(list '() b)   b]

    
    [(list (list c1) (list c2))
     ;=>
     (+/snafu-char c1 c2)]

    
    [(list (cons char1 rest1)
           (cons char2 rest2))
     ;=>
     (define char1+char2 (+/snafu-char char1 char2))
     (define base  (car char1+char2))
     (define carry (cdr char1+char2))
     
     (define rest1+carry       (+/snafu-reverse-char-list rest1 carry))
     (define rest1+rest2+carry (+/snafu-reverse-char-list rest1+carry rest2))

     (cons base rest1+rest2+carry)]))

; Sum two SNAFU numbers: 
(define (+/snafu snafu1 snafu2)
  (list->string
   (reverse
    (+/snafu-reverse-char-list
     (reverse (string->list snafu1))
     (reverse (string->list snafu2))))))


; Grab the input:
(define snafu-numbers (file->lines "input.txt"))

; Print the input snafu numbers and their decimal equivalents:
(for ([line snafu-numbers])
  (display line)
  (display " => ")
  (displayln (snafu->integer line)))

; Print the decimal amount:
(display "Decimal sum is: ")
(foldl + 0 (map snafu->integer snafu-numbers))

; Print the amount in snafu:
(display "SNAFU sum is: ")
(foldl +/snafu "0" snafu-numbers)




