
(require "asdf")

(load "~/quicklisp/setup.lisp")

(ql:quickload "trivia") ; for match
(ql:quickload "letrec") ; for letrec

(use-package :trivia)
(use-package :letrec)


; Grab the blueprint # from the command line
(defvar blueprint-num (parse-integer (nth 1 sb-ext:*posix-argv*)))


; The blueprints from the input file:
(defvar blueprints
'((1      4      4    ( 4   14 )  (   3   16))
  (2      3      3    ( 2   19 )  (   2   12))
  (3      4      4    ( 4   9  )  (   4   16))
  (4      4      3    ( 4   20 )  (   2   15))
  (5      3      4    ( 4   18 )  (   2   11))
  (6      4      4    ( 3   11 )  (   3   8))
  (7      2      3    ( 2   17 )  (   3   19))
  (8      4      3    ( 2   7  )  (   3   8))
  (9      3      4    ( 3   15 )  (   4   16))
  (10     3      3    ( 2   7  )  (   2   9))
  (11     4      4    ( 4   18 )  (   4   9))
  (12     4      4    ( 2   17 )  (   3   11))
  (13     4      3    ( 3   17 )  (   3   13))
  (14     4      3    ( 4   18 )  (   4   11))
  (15     4      4    ( 4   8  )  (   3   19))
  (16     4      3    ( 2   13 )  (   2   10))
  (17     4      4    ( 4   20 )  (   2   8))
  (18     3      3    ( 3   17 )  (   2   13))
  (19     4      3    ( 2   13 )  (   2   9 ))
  (20     2      3    ( 3   11 )  (   2   16 ))
  (21     3      4    ( 2   20 )  (   4   7 ))
  (22     2      4    ( 3   19 )  (   4   13 ))
  (23     3      3    ( 3   17 )  (   4   8 ))
  (24     3      3    ( 3   20 )  (   2   12 ))
  (25     2      2    ( 2   17 )  (   2   10 ))
  (26     3      3    ( 3   9  )  (   3   7 ))
  (27     4      4    ( 2   11 )  (   2   7 ))
  (28     4      3    ( 3   14 )  (   4   17 ))
  (29     4      4    ( 2   8  )  (   3   9 ))
  (30     3      4    ( 3   18 )  (   4   19 ))))


 
; Number of minutes -- 24 for part 1; 32 for part 2
(defvar time-limit 24)

; Sample blueprint 1:
(defvar sample-blueprint '(4 2 (3 14) (2 7)))

; Sample blueprint 2:
(defvar sample-blueprint-2 '(2 3 (3 8) (3 12)))


; The search state space is 
; 1: # ore available
; 2: # clay available
; 3: # obsidian available
; 4: # ore bots
; 5: # clay bots
; 6: # obsidian bots
; 7: minutes remaining

; Possible steps at every state:
; build an ore bot
; build a clay bot
; build an obsidian bot
; build a geode bot
; do nothing -- wait/build up resources


; Memoize this function into the hash-table cache:
(defun memoize/equal (cache fn)
  ;(let ((cache (make-hash-table :test #'equal)))
   (lambda (&rest args)
    (let ((val (gethash args cache)))
      (if val
          val
          (let ((val (apply fn args)))
             (setf (gethash args cache) val)
             val)))))

; For debugging -- let's us inspect visited states:
(defparameter state-cache (make-hash-table :test #'equal))

(defun print-state/time (n)
 (loop for key being the hash-keys of state-cache
       do 
       (if (= (- time-limit (nth 6 key)) n)
          (print key))))


; Generate a solver based on a blueprint:
(defun best-generator (blueprint)
 
 (match blueprint
    ((list ore-bot-ore 
           clay-bot-ore 
           (list obsidian-bot-ore obsidian-bot-clay) 
           (list geode-bot-ore geode-bot-obsidian))

     ;=>
     (defun next-states (ore clay obsidian ore-bots clay-bots obsidian-bots time-left)

       (block alpha 


       (let* 
        (

        ; helpful values during state-space calculations:
        (t-2     (- time-left 2))
        (t-1     (- time-left 1))

        ; upper bound on the total amount of obsidian we could have at t-2:
        (max-obsidian-at-t-2    (+ obsidian (/ (* t-1 t-2) 2) (* obsidian-bots t-2)))

        (can-make-ore-botp       (>= ore ore-bot-ore))

        (can-make-geode-botp     (and (>= ore geode-bot-ore)
                                      (>= obsidian geode-bot-obsidian)))

        ; conditions for when we don't need any more of a given bot type:
        (maxed-out-ore-botsp     (>= ore-bots (max ore-bot-ore clay-bot-ore obsidian-bot-ore geode-bot-ore)))

        (maxed-out-clay-botsp    (>= clay-bots obsidian-bot-clay))

        (maxed-out-obsidian-botsp  (>= obsidian-bots geode-bot-obsidian))

        )


       ; bail out if we can't make any more geode bots:
       (if (< max-obsidian-at-t-2 geode-bot-obsidian)
           (return-from alpha '()))


       (append 

         ; Make an ore bot
         (if (and can-make-ore-botp
                  (not maxed-out-ore-botsp)
                  (not can-make-geode-botp))
             (list (list nil 
                    (+ ore      ore-bots    (- ore-bot-ore))
                    (+ clay     clay-bots)
                    (+ obsidian obsidian-bots)
                    (+ 1 ore-bots)
                    clay-bots 
                    obsidian-bots
                    (- time-left 1)))
             (list))
 
         ; Make a clay bot
         (if (and (>= ore clay-bot-ore) 
                  (not maxed-out-clay-botsp)
                  (not can-make-geode-botp))
             (list (list nil 
                    (+ ore      ore-bots   (- clay-bot-ore))
                    (+ clay     clay-bots)
                    (+ obsidian obsidian-bots)
                    ore-bots 
                    (+ 1 clay-bots) 
                    obsidian-bots 
                    (- time-left 1)))
             (list))


         ; Make an obsidian bot
         (if (and (>= ore obsidian-bot-ore)
                  (>= clay obsidian-bot-clay)
                  (not maxed-out-obsidian-botsp)
                  (not can-make-geode-botp))
             (list (list nil 
                    (+ ore      ore-bots    (- obsidian-bot-ore))
                    (+ clay     clay-bots   (- obsidian-bot-clay))
                    (+ obsidian obsidian-bots)
                    ore-bots 
                    clay-bots
                    (+ 1 obsidian-bots)
                    (- time-left 1)))
             (list))


         ; Make a geode bot
         (if can-make-geode-botp
             (list (list 1 
                    (+ ore      ore-bots      (- geode-bot-ore))
                    (+ clay     clay-bots)
                    (+ obsidian obsidian-bots (- geode-bot-obsidian))
                    ore-bots 
                    clay-bots
                    obsidian-bots
                    (- time-left 1)))
             (list))


         ; Make no bots
         (list (list nil
                (+ ore ore-bots)
                (+ clay clay-bots)
                (+ obsidian obsidian-bots)
                ore-bots
                clay-bots
                obsidian-bots
                (- time-left 1)))


          ))))


     (letrec ((best (memoize/equal state-cache (lambda (&rest state)
                       (block best-top
                       ;(print "handling: ")
                       ;(print state)

                       (match state
                         ((list ore clay obsidian ore-bots clay-bots obsidian-bots time-left)
                          ;=>
                          (cond
                            ((<= time-left 0)   
                             ;=>
                             0) 
                 
                            (t 
                             ;=>
                             (let ((next (apply #'next-states state)))
                                (apply #'max (cons 0 (map 'list #'score next))))))))))))

               (score (lambda (geode*state)
                        (let ((geodep    (car geode*state))
                              (state     (cdr geode*state))
                              (time-left (nth 7 geode*state)))
                          (+ (if geodep time-left 0) (apply #'best state))))))

        #'best))))


; Grab the specified blueprint:
(defparameter blueprint (cdr (assoc blueprint-num blueprints)))

;(defparameter solver (best-generator sample-blueprint-2))

; Generate the solver for this blueprint:
(defparameter solver (best-generator blueprint))

(print "running on: ")
(print blueprint)

(print "maximum geodes: ")

; Solve for the max number of geodes:
(defvar max-geodes (funcall solver 0 0 0 1 0 0 time-limit))

(print max-geodes)


; Save the results of this run:
(with-open-file (str (concatenate 'string "./results/output-" (write-to-string blueprint-num) ".txt")
                     :direction :output
                     :if-exists :supersede
                     :if-does-not-exist :create)
  (format str (write-to-string max-geodes)))
