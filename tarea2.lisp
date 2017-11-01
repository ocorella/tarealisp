;;------------------------------------------------------------------------------
;;
;; Universidad de Costa Rica
;; Escuela de Ciencias de la Computación e Informática
;; CI-1441 Paradigmas Computacionales
;; II-2017, Prof. Dr. Alvaro de la Ossa Osegueda
;;
;;------------------------------------------------------------------------------
;;
;; tarea2.lisp -- Segunda Tarea Programada: LISP,
;;------------------------------------------------------------------------------
;;
;; 1. Funciones interesantes de procesamiento de árboles y conjuntos
;;
;; Suponga que existe una lista de acceso global de la forma ( .. (o p v) .. ),
;; denominada "propiedades", en la que cada terna (o p v) representa una
;; propiedad p con valor v de un objeto o. Supongo además que para cada objeto y
;; propiedad pueden existir en esa lista varias ternas con valores distintos. Lo
;; que no puede haber son ternas repetidas.
;;
;; 1.1 (bpp N A) -> el subárbol de A cuya raíz es N, nil si no existe
;;(bpp 'x '(a (b c) d (x e f) c))
;; Devuelve a b c d x (x e f)
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

;; 1.2 (bap N A) -> el subárbol de A cuya raíz es N, nil si no existe
;;(bap 'x '(a b (c d e) (f (g h) i)))
;; Devuelve d; despliega a b c f d
(defun bap (param arbol)
	(let ((heads (getHeads arbol param))
				(tails (getTails arbol)))
		(cond
			((member param heads) (and (printUntil param heads) (getTree param arbol)))
			(t (and (bap param tails) (printUntil param heads)))
		)
	)
)
(defun printUntil (N L)
	(cond ((null L) nil)
				((eq (car L) N) (print (car L)))
				(t (and (print (car L)) (printUntil N (cdr L))))
	)
)

(defun getHeads (A N)
	(cond ((null A) nil)
				((null (car A) ) (getHeads (cdr A) N))
				( (atom (car a)) (cons (car a) (getHeads (cdr A) N)))
				( (not (atom (car a))) (cons (caar a) (getHeads (cdr A) N)))
				(t (getHeads (cdr A) N))
	)
)

(defun getTails (A)
	(cond ((null A) nil)
				((null (car A) ) (getTails (cdr A)))
				( (atom (car a)) (cons '() (getTails (cdr A))))
				((not (atom (car a))) (cons (cdar a) (getTails (cdr A))))
				(t (getTails (cdr A)))
	)
)

(defun getTree (N L)
	(cond
				((atom (car L)) (cond ((eq N (car L)) (car A))
															(t(getTree N (cdr L)))
												)
				)
				(t 							(cond ((eq N (caar L)) (car A))
															(t(getTree N (cdr L)))
												)
				)

	)
)

;; 1.3 (potencia C) el conjunto potencia de C
;;eg (potencia '(1 2 3 4))
;; devuelve: (NIL (4) (3) (3 4) (2) (2 4) (2 3) (2 3 4) (1) (1 4) (1 3) (1 3 4) (1 2) (1 2 4) (1 2 3) (1 2 3 4))
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

;; 1.4 (cartesiano A B) el producto cartesiano de A y B
;;eg (cartesiano '(1 2 3) '(3 4 5))
;;devuelve: ((1 3) (1 4) (1 5) (2 3) (2 4) (2 5) (3 3) (3 4) (3 5))

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

;devuelve todas las combinaciones entre una lista y un elemento
(defun combinaciones (elemento lista)
	(cond
		((null lista) nil)
		(t
			(cons (list elemento (car lista)) (combinaciones elemento (cdr lista)))
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


;;Encripta una hilera dada un alfabeto de entrada y otro de salida
;;(decripta '(o s c a r a p u) '(a b c d e f g h i j k l m n o p q r s t u v w x y z) '(a b c d e f g h i j k l m n o p q r s t u 0 1 2 3 4 5 6 7 8 9))
;; devuelve Hilera encriptada = (O S 7 0 H Q A F) Estado final = (U . F)
(defun decripta (H Ae As Ef))
	(defparameter AlfabetoEntrada Ae)
	(defparameter AlfabetoSalida As)
	(defparameter Hilera H)
	(defparameter EstadoFinal Ef)
	(defparameter Resultado '())

	(recorrer (list-length H))
	(write-line "")
	(format nil "Hilera encriptada = ~S Estado final = ~S" Resultado (cons (car AlfabetoEntrada) (car AlfabetoSalida)))
)

(defun girarAEstadoInicial
	(cond((not (and (equal (car EstadoFinal) (car AlfabetoEntrada)) (equal (cadr EstadoFinal) (car AlfabetoSalida)))
			(setq AlfabetoEntrada (rotarN AlfabetoEntrada 1))
			(setq AlfabetoSalida (rotarN AlfabetoSalida 1))
			(girarLista valor)
		)
	)
)
