;; Blackjack, by Al Sweigart al@inventwithpython.com

;;     Rules:
;;       Try to get as close to 21 without going over.
;;       Kings, Queens, and Jacks are worth 10 points.
;;       Aces are worth 1 or 11 points.
;;       Cards 2 through 10 are worth their face value.
;;       (H)it to take another card.
;;       (S)tand to stop taking cards.
;;       On your first play, you can (D)ouble down to increase your bet
;;       but must hit exactly one more time before standing.
;;       In case of a tie, the bet is returned to the player.
;;       The dealer stops hitting at 17.
;; Money: 5000
;; How much do you bet? (1-5000, or QUIT)
;; > 400
;; Bet: 400

;; DEALER: ???
;;  ___   ___
;; |## | |2  |
;; |###| | ♥ |
;; |_##| |__2|

;; PLAYER: 17
;;  ___   ___
;; |K  | |7  |
;; | ♠ | | ♦ |
;; |__K| |__7|


;; (H)it, (S)tand, (D)ouble down
;; > h
;; You drew a 4 of ♦.
;; --snip--
;; DEALER: 18
;;  ___   ___   ___
;; |K  | |2  | |6  |
;; | ♦ | | ♥ | | ♠ |
;; |__K| |__2| |__6|

;; PLAYER: 21
;;  ___   ___   ___
;; |K  | |7  | |4  |
;; | ♠ | | ♦ | | ♦ |
;; |__K| |__7| |__4|

;; You won $400!
;; --snip—
(defun gen-random(en)
  (+ 1 (random en)))

;; return a number which didn't pick before
(let ((picked-alist '())) 
  (defun gen-random-number(en)
    (let ((rn (gen-random en)))
      (if (assoc rn picked-alist)
	  (gen-random-number en)
	  (progn
	    (push (cons rn 1) picked-alist)
	    rn))))
  (defun print-alist ()
    picked-alist)
  
  (defun shuffle-cards()
    (setq picked-alist '())))


(defparameter *suite-alist*
  (list (cons 1 9829)
	(cons 2 9830)
	(cons 3 9824)
	(cons 4 9827)))

(defparameter *rank-alist*
  (list (cons 1 'A)
	(cons 2 2)
	(cons 3 3)
	(cons 4 4)
	(cons 5 5)
	(cons 6 6)
	(cons 7 7)
	(cons 8 8)
	(cons 9 9)
	(cons 10 10)
	(cons 11 'J)
	(cons 12 'Q)
	(cons 13 'K)))

(defun get-random-card()
  (let ((rn (gen-random-number 51)))
    (multiple-value-bind (quo rem) (floor rn 13)
      (let ((suite (code-char (cdr (assoc (+ 1 quo) *suite-alist*))))
	    (rank (cdr (assoc (+ 1 rem) *rank-alist*))))
	(format t "|~a ~a ~a|" suite rank suite)))))

(defun print-card(n x)
   (multiple-value-bind (quo rem) (floor n 13)
      (let ((suite (code-char (cdr (assoc (+ 1 quo) *suite-alist*))))
	    (rank (cdr (assoc (+ 1 rem) *rank-alist*))))
	(if x
	    (format t "|~a ~a ~a|" suite rank suite)
	    (format t "|x x x|"))))) 
	    
;; ignore one value from the multiple-value-bind
;; https://stackoverflow.com/a/23317812
(defun card-value(n)
  (multiple-value-bind (_ rem) (floor n 13)
    (declare (ignore _))
    (let ((card-value (+ 1 rem)))
	(if (>= card-value 10)
	    10
	    card-value))))
	

(defun deal(alist)
  (list
   ;; deal
   #' (lambda() (let* ((rn (gen-random-number 51))
		       (rvalue (card-value rn)))
		  (format t "|~a ~a|" rn rvalue)		 
		  (push (cons rn rvalue) alist)))
      ;; sum
      #' (lambda() (reduce
		    (lambda (acc x)
		      (let* ((k (cdr x)))
			(if k
			    (+ acc k)
			    acc)))
		    alist
		    :initial-value 0))))
  

(setq deal-p (deal '())
      deal-d (deal '()))

(defun deal-player()
  (funcall (car deal-p)))

(defun sum-player()
  (funcall (second deal-p)))

(defun deal-dealer()
  (funcall (car deal-d)))

(defun sum-dealer()
  (funcall (second deal-d)))


