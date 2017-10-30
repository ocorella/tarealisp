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

;;Encripta una hilera dada un alfabeto de entrada y otro de salida
;;(encripta '(o s c a r a p u) '(a b c d e f g h i j k l m n o p q r s t u v w x y z) '(a b c d e f g h i j k l m n o p q r s t u 0 1 2 3 4 5 6 7 8 9))
;; devuelve Hilera encriptada = (O S 7 0 H Q A F) Estado final = (U . F)

(defun encripta (H Ae As)
	(defparameter AlfabetoEntrada Ae)
	(defparameter AlfabetoSalida As)
	(defparameter Hilera H)
	(defparameter Resultado '())
	(recorrer (list-length H))
	(write-line "")
	(format nil "Hilera encriptada = ~S Estado final = ~S" Resultado (cons (car AlfabetoEntrada) (car AlfabetoSalida)))
)

;;Recorre la lista a encriptar
(defun recorrer (cantidad)
	(cond((> cantidad 0)
		(girarLista (car Hilera))
		(setq Hilera (rotarN Hilera 1))
		(setq Resultado (append Resultado (list(car AlfabetoSalida))))
		(recorrer (1- cantidad))
		)
	)
)

;;Rota hasta llegar al valor
(defun girarLista (valor)
	(cond((not (equal valor (car AlfabetoEntrada)))
			(setq AlfabetoEntrada (rotarN AlfabetoEntrada 1))
			(setq AlfabetoSalida (rotarN AlfabetoSalida 1))
			(girarLista valor)
		)
	)
)

;;Rota la lista n veces
(defun rotarN (lista n)
	(cond((> n 0)
			(rotarN (append (cdr lista) (list(car lista))) (1- n))
		)
		(t lista)
	)
)
