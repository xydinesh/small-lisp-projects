;; Bagels, a deductive logic game.
;; I am thinking of a 3-digit number. Try to guess what it is.
;; Here are some clues:
;; When I say:    That means:
;;   Pico         One digit is correct but in the wrong position.
;;   Fermi        One digit is correct and in the right position.
;;   Bagels       No digit is correct.
;; I have thought up a number.
;;  You have 10 guesses to get it.
;; Guess #1:
;; > 123
;; Pico
;; Guess #2:
;; > 456
;; Bagels
;; Guess #3:
;; > 178
;; Pico Pico
;; --snip--
;; Guess #7:
;; > 791
;; Fermi Fermi
;; Guess #8:
;; > 701
;; You got it!
;; Do you want to play again? (yes or no)
;; > no
;; Thanks for playing!
;;

(defun gen-random(st en)
  (do ((x 1 (random en)))
      ((> x st) x)))

(defun find-element (e lista index)
  (cond
    ((null lista) (values nil index))
    ((= e (car lista))
     (values t index))
    (t (find-element e (cdr lista) (+ 1 index)))))


(defun compare-lists(gl il index bagels)
  (let ((match nil))
    (cond
      ((and (null il) (eq bagels nil)) (print "Bagels"))
      (t
       (multiple-value-bind (state i) (find-element (car il) gl 0)
	 (cond
	   ((and (eq t state) (eq index i))
	    (setq match t)
	    (print "Pico"))
	   ((eq t state)
	    (setq match t)
	    (print "Fermi")))
	 (compare-lists gl (cdr il) (+ 1 index) match))))))


(defun match-numbers(g i)
  (let ((genl (map 'list #'digit-char-p (prin1-to-string g)))
	(iteml (map 'list #'digit-char-p (prin1-to-string i))))
    (compare-lists genl iteml 0 nil)))


(defun play-bagels()
  (let ((y (gen-random 100 1000)))
    (do ((x 1 (+ 1 x)))
	((> x 2))
      (format t "~%~D>:" x)
      (let ((item (read)))
	(if (null item) (return)
            (match-numbers y item))))))
