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
;; Oscar Corella Quirós B32080
;; Ricardo Apú Chinchilla B40399


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
(defun bap (N L)
	(cond
				((null L) nil)
				((atom (car L)) (cond ((eq N (car L)) (list (car L)))
															(t (print (car L)) (bap N (cdr L)))
												)
				)
				(t 							(cond ((eq N (caar L)) (car L))
															(t (print (caar L)) (bap N (append (cdr L) (cdar L))))

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
;;"Hilera encriptada = (Q M C J 2 O 9 4) Estado final = (U . 4)"

(defun encripta (H Ae As)
	(defparameter AlfabetoEntrada Ae)
	(defparameter AlfabetoSalida As)
	(defparameter Hilera H)
	(defparameter Resultado '())
	(setq AlfabetoSalida (reversa As))
	(print AlfabetoEntrada)
	(print AlfabetoSalida)
	(recorrer (list-length H))
	(write-line "")
	(format nil "Hilera encriptada = ~S Estado final = ~S" Resultado (cons (car AlfabetoEntrada) (car AlfabetoSalida)))
)
(defun reversa (l)
   (cond ((null l) nil)
         (t (append (reversa (cdr l)) (cons (car l) nil)))))

;;Recorre la lista a encriptar
(defun recorrer (cantidad)
	(cond((> cantidad 0)
		(girarLista (car Hilera))
		(setq Hilera (rotarN Hilera 1))
		(setq Resultado (append Resultado (list(car AlfabetoSalida))))
		(print AlfabetoEntrada)
		(print AlfabetoSalida)
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
(defun rotar (lista )
	(append (cdr lista) (list(car lista)))
)

;;Encripta una hilera dada un alfabeto de entrada y otro de salida
;;(decripta '(Q M C J 2 O 9 4) '(a b c d e f g h i j k l m n o p q r s t u v w x y z) '(a b c d e f g h i j k l m n o p q r s t u 0 1 2 3 4 5 6 7 8 9) '(U . 4))
;; devuelve Hilera decriptada = (OSCARAPU)
(defun decripta (H Ae As Ef)
	(defparameter AlfabetoEntrada2 Ae)
	(defparameter AlfabetoSalida2 As)
	(defparameter Hilera2 H)
	(defparameter EstadoFinal Ef)
	(defparameter Resultado2 '())
	(setq AlfabetoSalida2 (reversa As))
	(girarAeEstadoInicial)
	(girarAsEstadoInicial)
	(print AlfabetoEntrada2)
	(print AlfabetoSalida2)
	(write-line "")
	(recorrerDecripta (reversa H) AlfabetoEntrada2 AlfabetoSalida2)
	(write-line "")
	(format nil "Hilera decriptada = ~S" Resultado2)
)

(defun girarAeEstadoInicial ()
	(cond(
			(not (eq (car EstadoFinal) (car AlfabetoEntrada2)))
			(setq AlfabetoEntrada2 (rotarN AlfabetoEntrada2 1))
			(girarAeEstadoInicial)
			)
	)
)
(defun girarAsEstadoInicial ()
	(cond(
			(not(eq (cdr EstadoFinal) (car AlfabetoSalida2)))
			(setq AlfabetoSalida2 (rotarN AlfabetoSalida2 1))
			(girarAsEstadoInicial)
			)
	)
)

;;Recorre la lista a encriptar
(defun recorrerDecripta (H Ae As)
	(cond ((null H) nil)
				(t (cond
							((eq (car H) (car As))
								(setq Resultado2 (append Resultado2 (list(car Ae))))
								(print Ae)
								(print As)
								(write-line "")
								(recorrerDecripta (cdr H) Ae As)
					   )
					(t(recorrerDecripta H (rotar Ae) (rotar As) ))
				)
			)
	)
)
