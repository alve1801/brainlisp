(defun inc lambda (val) ((val)))

(defun dec lambda (val) (car val))

(defun moar lambda (a b) (cond
	((eq a nil) nil)
	((eq b nil) t)
	(t (moar (dec a) (dec b)))
))

(defun get_at lambda (index list)
	(cond
		((eq index nil) (car list))
		(t (get_at (dec index) (cdr list)))
	)
)

(defun set_at lambda (val index list)
	(cond
		((eq index nil) (cons val (cdr list)))
		(t (cons (car list) (at val (dec index) (cdr list))))
	)
)

(defun len lambda (list) (cond (list (inc (len (cdr list))))(t nil)))

(defun append lambda (list) (cond (list (cons (car list) (append (cdr list)))) (t (nil))))

(defun fwd lambda (prog pp val) (cond (val (cond
	((eq (get_at pp prog) (quote [)) (fwd prog (inc pp) (inc val)))
	((eq (get_at pp prog) (quote ])) (fwd prog (inc pp) (dec val)))
	(t (fwd prog (inc pp) val))
)) (t pp)))

(defun bck lambda (prog pp val) (cond (val (cond
	((eq (get_at pp prog) (quote [)) (fwd prog (inc pp) (dec val)))
	((eq (get_at pp prog) (quote ])) (fwd prog (inc pp) (inc val)))
	(t (fwd prog (dec pp) val))
)) (t pp)))

(defun bf_eval lambda (prog pp stack sp)
	(cond
		((moar pp (len prog)) stack)
		((eq (get_at pp prog) (quote +)) (bf_eval prog (inc pp) (set_at (inc (get_at sp stack)) sp stack) sp))
		((eq (get_at pp prog) (quote -)) (bf_eval prog (inc pp) (set_at (dec (get_at sp stack)) sp stack) sp))
		((eq (get_at pp prog) (quote <)) (bf_eval prog (inc pp) stack (dec sp)))
		((eq (get_at pp prog) (quote >)) (bf_eval prog (inc pp) (cond ((moar sp (len stack)) (append stack)) (t stack)) (inc sp)))
		((eq (get_at pp prog) (quote [)) (cond
			((get_at sp stack) (bf_eval prog (inc pp) stack sp))
			(t ((bf_eval prog (fwd prog pp (nil)) stack sp)))
		))
		((eq (get_at pp prog) (quote ])) (cond
			((get_at sp stack) ((bf_eval prog (bck prog pp (nil)) stack sp)))
			(t (bf_eval prog (inc pp) stack sp))
		))
		(t stack)
	)
)

(defun brainfuck lambda (prog) (bf_eval prog nil (nil) nil))

(brainfuck (quote (+++++++[>+++++++<-]>.)))
