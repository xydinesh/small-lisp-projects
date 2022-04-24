;; How many birthdays shall I generate? (Max 100)
;; > 23
;; Here are 23 birthdays:
;; Oct 9, Sep 1, May 28, Jul 29, Feb 17, Jan 8, Aug 18, Feb 19, Dec 1, Jan 22,
;; May 16, Sep 25, Oct 6, May 6, May 26, Oct 11, Dec 19, Jun 28, Jul 29, Dec 6,
;; Nov 26, Aug 18, Mar 18
;; In this simulation, multiple people have a birthday on Jul 29
;; Generating 23 random birthdays 100,000 times...
;; Press Enter to begin...
;; Let's run another 100,000 simulations.
;; 0 simulations run...
;; 10000 simulations run...
;; --snip--
;; 90000 simulations run...
;; 100000 simulations run.
;; Out of 100,000 simulations of 23 people, there was a
;; matching birthday in that group 50955 times. This means
;; that 23 people have a 50.95 % chance of
;; having a matching birthday in their group.
;; That's probably more than you would think!


;; First design a program to run a simulation with numbers from 1 to 100


;; A great introductory article discussing usage for reduce and alists.
;; https://stackoverflow.com/questions/6050033/elegant-way-to-count-items

(defparameter *months-alist*
  (list (cons 'Jan 31)
	(cons 'Feb 28)
	(cons 'Mar 31)
	(cons 'Apr 30)
	(cons 'May 31)
	(cons 'June 30)
	(cons 'July 31)
	(cons 'Aug 31)
	(cons 'Sept 30)
	(cons 'Oct 31)
	(cons 'Nov 30)
	(cons 'Dec 31)))

(defun gen-random(st en)
  (do ((x 1 (random en)))
      ((> x st) x)))

(defun gen-list(count end)
  (loop repeat count collect (gen-random 1 end)))

(defun reverse-elements(elements)
        (do ((x elements (cdr x))
             (y '() (cons (car x) y)))
            ((endp x) y)))

(defun count-elements(elements)
  (reduce
   (lambda (acc x)
   (let* ((pair (assoc x acc))
	  (count (cdr pair)))
     (if pair
	 (cons (cons x (+ 1 count)) acc)
	 (cons (cons x 1) acc))))
   elements
   :initial-value '()))

(defun count-elements2(elements)
  (reduce
   (lambda (acc x)
     (incf (gethash x acc 0))
     acc)
   elements
   :initial-value (make-hash-table)))

(defun get-max-occurances(elements)
  (reduce
   (lambda (acc x)
     (let* ((k (cdr x)))
       (if (> k acc)
	   k
	   acc)))
   elements
   :initial-value 0))


(defun gen-random-bday()
  (let* ((pair (nth (random 12) *months-alist*))
	 (month (car pair))
	 (date (cdr pair)))
    (cons month (gen-random 1 date))))

(defun gen-random-bday-list(count)
  (loop repeat count collect (gen-random-bday)))

;; When keys are strings in an alist we have to pass :test parameter
;; https://stackoverflow.com/questions/20423489/can-i-use-assoc-when-the-keys-are-strings
(defun count-bdays (elements)
  (reduce
   (lambda (acc x)
     (let* ((month (car x))
	     (date (cdr x))
	     (bday (format nil "~s ~s" month date))
	     (pair (assoc bday acc :test #'equal))
	     (count (cdr pair)))
       (if pair
	   (cons (cons bday (1+ count)) acc)
	   (cons (cons bday 1) acc))))
   elements
   :initial-value '()))

(defun birthday-simulation()
  (format t "~%>")
  (let ((item (read))
	(acc 0.0)
	(total 100000))
    (do ((x 1 (1+ x)))
	((> x total) (format nil "~D pepole have ~D% chance of matching birthdays" item (/ (* acc 100.0) total)))
      (if (= (mod x 10000) 0)
	  (format t "~A ~A ~%" x "Simulations completed"))
      (if (> (get-max-occurances (count-bdays (gen-random-bday-list item))) 1)
	  (incf acc))
	  acc)))

