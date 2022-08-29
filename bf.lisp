(defun get_at lambda (index list)
	(cond
		((eq index 0) (car list))
		(t (get_at (- index 1) (cdr list)))
	)
)

(defun set_at lambda (val index list)
	(cond
		((eq index 0) (cons val (cdr list)))
		(t (cons (car list) (at val (- index 1) (cdr list))))
	)
)

(defun len lambda (list) (cond (list (+ 1 (len (cdr list))))(t 0)))

(defun append lambda (list) (cond (list (cons (car list) (append (cdr list)))) (t 0)))

(defun fwd lambda (prog pp val) (cond (val (cond
	((eq (get_at pp prog) (quote [)) (fwd prog (+ pp 1) (+ val 1)))
	((eq (get_at pp prog) (quote ])) (fwd prog (+ pp 1) (- val 1)))
	(t (fwd prog (+ pp 1) val))
)) (t pp)))

(defun bck lambda (prog pp val) (cond (val (cond
	((eq (get_at pp prog) (quote [)) (fwd prog (+ pp 1) (- val 1)))
	((eq (get_at pp prog) (quote ])) (fwd prog (+ pp 1) (+ val 1)))
	(t (fwd prog (- pp 1) val))
)) (t pp)))

(defun bf_eval lambda (prog pp stack sp)
	(cond
		((> pp (len prog)) stack)
		((eq (get_at pp prog) (quote +)) (bf_eval prog (+ pp 1) (set_at (+ (get_at sp stack) 1) sp stack) sp))
		((eq (get_at pp prog) (quote -)) (bf_eval prog (+ pp 1) (set_at (- (get_at sp stack) 1) sp stack) sp))
		((eq (get_at pp prog) (quote <)) (bf_eval prog (+ pp 1) stack (- sp 1)))
		((eq (get_at pp prog) (quote >)) (bf_eval prog (+ pp 1) (cond ((> sp (len stack)) (append stack)) (t stack)) (+ sp 1)))
		((eq (get_at pp prog) (quote [)) (cond
			((get_at sp stack) (bf_eval prog (+ pp 1) stack sp))
			(t ((bf_eval prog (fwd prog pp 1) stack sp)))
		))
		((eq (get_at pp prog) (quote ])) (cond
			((get_at sp stack) ((bf_eval prog (bck prog pp 1) stack sp)))
			(t (bf_eval prog (+ pp 1) stack sp))
		))
		((eq (get_at pp prog)) (quote X) (bf_eval prog (+ pp 1) stack sp))
	)
)

(defun brainfuck lambda (prog) (bf_eval prog 0 (0) 0))

(brainfuck (quote (+++++++[>+++++++<-]>.)))


(comment assuming prog is a lisp of valid brainfuck instructions, (brainfuck prog) returns the stack after prog is done executing)
(comment valid meaning that its brackets are balanced and it doesnt go into negative tape space)
(comment also ive been referring to it as a stack when its actually a tape, please ignore that)
