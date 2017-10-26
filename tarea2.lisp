(defun bpp (param arbol)
	(cond
		((not (atom param)) nil) ((atom arbol) nil) ((null arbol) nil)
		(t (print (car arbol)))
	)
	(cond
		((equal (car arbol) param) arbol) (t (bpp* param (cdr arbol)))
	)
)

(defun bpp* (param arbol)
	(cond
		((null arbol) nil)
		(
			(atom (car arbol))
			(let((sub (bpp param (cons (car arbol) nil))))
				(cond (sub sub)
					(t (bpp* param (cdr arbol)))
				)
			)
		)
		(t (let((s (bpp param (car arbol))))
				(cond
					(s s)
					(t (bpp* param (cdr arbol)))
				)
			)
		)
	)
)

(defun potencia(lista)
	(cond
		((atom lista) nil)
		((null lista) nil)
		(t (potencia* lista) )
	)
)
(defun potencia*(lista)
	(if (null lista) '(nil)
		(let ((resultado (potencia* (cdr lista))))
			(append resultado (mapcar #'(lambda (x) (cons (car lista) x)) resultado))
		)
	)
)


(defun cartesiano (A B)
	(cond
		((atom A) nil)
		((atom B) nil)
		((null A) nil)
		((null B) nil)
		(t
			(append (combinaciones (car A) B) (cartesiano (cdr A) B))
		)
	)
)
