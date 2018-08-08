;Funcion principal que recibe un programa en pseudo C y una lista de datos
(defun run (prg datos &optional mem)
	(if (null prg) nil
		(if (eq (car (car prg)) 'int) (run (cdr prg) datos (insertarEnMem (cdr (car prg)) mem))
			(if (eq (car (car prg)) 'main) (ejecutar (car (cdr (car prg))) datos mem) 'ERROR)	
		)
	)
)


;Recibe una lista con el codigo de C a ejecutar y devuelve el resultado de la ejecuccion
(defun ejecutar (prg datos mem &optional salida)
	(print mem)
	(if (null prg) (reverse salida)
		(cond
			( (esFuncion prg 'scanf) (ejecutar (cdr prg)(cdr datos) (asociar (car (cdr (car prg))) (car datos) mem) salida))
			( (esFuncion prg 'printf) 
				(if (esVector (car (cdr (car prg))) mem)
					(ejecutar (cdr prg) datos mem (cons (evaluar (car (cdr (car prg))) mem (car (cdr (cdr (cdr (car prg)))))) salida) ) 
					(ejecutar (cdr prg) datos mem (cons (evaluar (car (cdr (car prg))) mem) salida) ) ))
			
			
			( (esFuncion prg  'if) (procesar_if prg datos mem salida ))
			( (esFuncion prg  'while)(procesar_while prg datos mem salida )) 
			( (esAsignacion (car prg) mem) (ejecutar (cdr prg) datos (asignacion (car prg) mem) salida ) )
			(  T (list 'Error_de_sintaxis  prg ))	
		)
	)
)


;Compara el nombre de una funcion o un ciclo con la funcion a ejecutar 
(defun esFuncion (prg funcion)
	(equal (car (car prg)) funcion)
)

;Recibe las asignaciones previas al main y las almacena en la memoria
(defun insertarEnMem (var mem)
	(if (null var) nil
		(if (equal  (nth 1 var ) '[])	(asociar (nth 0 var) (nth 3 var) (insertarEnMem (cdr (cdr (cdr (cdr var)))) mem))
			(if (equal (length var) 1) 	(asociar (car var) 0  mem)
				(if (equal (nth 1 var) '=)		(asociar (nth 0 var) (nth 2 var)(insertarEnMem (cdr (cdr (cdr var))) mem))
					(asociar (car var) (car (last var)) (insertarEnMem (cdr var) mem))
				)
			)
		)
	)
)

;Busca la variable en la memoria y la modifica si existe o la agrega si no existe
(defun asociar (var valor memoria &optional indice)
	; Si no lo encontre en la memoria lo agrego
	(if (null indice)
		(if (null memoria) (cons (list var valor) memoria)
			; Si existe la modifico sino sigo buscando
			(if (equal (car (car memoria)) var) (cons (list var valor) (cdr memoria))
				(cons (car memoria) (asociar var valor (cdr memoria)))
			)
		)
		(if (equal (car (car memoria)) var) 
				;Busco el valor que contiene ese indice para modificarlo
				(cons (modificarPorIndice (car memoria) valor indice) (cdr memoria))
				(cons (car memoria) (asociar var valor (cdr memoria)))
			)
	)
		
)

; vector= (v (1 2 3))
(defun modificarPorIndice (vector valor indice)
	(cond
		((eq indice 0 )	(list (car vector) (cons valor (cdr (car(cdr vector))))))
		((eq indice 1 )	(list (car vector) (list (car (car (cdr vector))) valor (car (cdr (cdr (car (cdr vector))))))))
		((eq indice 2 )	(list (car vector) (list (car (car (cdr vector))) (car (cdr (car (cdr vector)))) valor)))
		( T 'Excede_limite)
	)
)

;Reemplaza los nombres de las variables por su valor real 
(defun reemplazarVars (prg mem &optional indice)
	(if (null prg) nil
		(if (listp (car prg))	(cons (reemplazarVars (car prg) mem) (reemplazarVars (cdr prg) mem))
			(if (existeVariable (car prg) mem) 
					(if (esVector (car prg) mem)	(cons (buscarVariable (car prg) mem (car (cdr (cdr prg)))) (reemplazarVars (cdr (cdr (cdr (cdr prg)))) mem))
						(cons (buscarVariable (car prg) mem) (reemplazarVars (cdr prg) mem)))
					(cons (car prg) (reemplazarVars (cdr prg) mem)) 
			)
		)
	)
)

;Recibe una expresion en C y devuelve la expresion equivalente en Lisp
(defun convertirExpLisp (expr ops vars)
	(if (null expr) (componerExpresion ops vars)
		(if (esOperador (car expr))
			(if (> (length ops) 0)
				;Si ya tengo un operador armo la expresion en base a la prioridad matematica
				(if (< (obtenerPrioridad (car expr)) (obtenerPrioridad (car ops)))
					(convertirExpLisp (cdr expr) (cons (car expr)(cdr ops)) 
								(cons (list (car ops) (car (cdr vars)) (car vars)) (cdr (cdr vars)) )
					)
					(convertirExpLisp (cdr expr) (cons (car expr) ops) vars)
				)
				(convertirExpLisp (cdr expr) (cons (car expr) ops) vars)
			)
			(if (atom (car expr))
				(convertirExpLisp (cdr expr) ops (cons (car expr) vars))
				(convertirExpLisp (cdr expr) ops (cons (car (convertirExpLisp (car expr) nil nil)) vars))
			)
		)	
	)
)


(defun componerExpresion (ops vars)
	(if (null ops) vars
		;Caso particular de comparacion en C 
		(cond	((eq (car ops ) '==)	(componerExpresion (cdr ops) (cons (list 'eq (car (cdr vars)) (car vars) ) (cdr (cdr vars)))))
					(T 	(componerExpresion (cdr ops) (cons (list (car ops) (car (cdr vars)) (car vars)) (cdr (cdr vars)) )))
		))
)


;Recibe un elemento y devuelve T o nil en caso de que el elemento sea un operador matematico
(defun esOperador (elemento)
	(if (null elemento) nil
		(cond 
			((equal elemento '+) T)
			((equal elemento '*) T)
			((equal elemento '-) T)
			((equal elemento '/) T)
			((equal elemento '<) T)
			((equal elemento '>) T)
			((equal elemento '<=) T)
			((equal elemento '>=) T)
			((equal elemento '==) T)
			((equal elemento '=) T)
			( T nil)
		)
	)
)


;Le da prioridad a los operadores matematicos
(defun obtenerPrioridad (op)
	(if (null op) nil
		(cond
			((equal op '+) 2)
			((equal op '*) 3)
			((equal op '-) 2)
			((equal op '/) 3)
			((equal op '<) 1)
			((equal op '>) 1)
			((equal op '<=) 1)
			((equal op '>=) 1)
			( T 0)
		)
	)
)


;Evalua una expresion lisp, devolviendo el valor de la variable, un numero literal o una expresion V o F 
(defun evaluarLisp (prg mem &optional indice)
	(if (atom prg) 
		(if (numberp prg)	prg
			(if (and (esVector prg mem) (null indice))	(buscarVariable prg mem (car (cdr (cdr prg))))
				(buscarVariable prg mem indice))
		)
		(eval (car (convertirExpLisp (reemplazarVars prg mem) nil nil)))
	)
)


;Convierte los null o true en valores booleanos
(defun filtrarNilTrue (resultado)
	(cond
		((null resultado) 0)
		((equal resultado T) 1)
		(T resultado)
	)
)


;Devuelve el valor de una variable si se manda un atomo, 0 si la expresion evaluada es nil, 1 si la expresion evaluada es True
(defun evaluar (prg mem &optional indice)
	(print indice)
	(filtrarNilTrue (evaluarLisp prg mem indice))
)


;Busca la variable en memoria, si no la encuentra devuelve error
(defun buscarVariable (var mem &optional indice)
	(if (null mem) 'ERROR_VARIABLE_NO_DECLARADA
		(if (equal (car (car mem)) var) 
			;Si el nombre de la variable es igual analizo si es o no un vector
			(if (null indice)	(car (cdr (car mem)))
				(cond
					((eq indice 0 )	(car (car (cdr (car mem)))))
					((eq indice 1 )	(car (cdr (car (cdr (car mem))))))
					((eq indice 2 )	(car (cdr (cdr (car (cdr (car mem)))))))
					( T 'Excede_limite)
				)
			)
			(buscarVariable var (cdr mem))
		) 
	)
)


;Si existe la variable en memoria devuelve true sino devuelve false
(defun existeVariable (var mem)
	(if (null mem) nil
		(if (equal (car (car mem)) var) T
			(existeVariable var (cdr mem))
		)
	)
)


;Si es un vector devuelve T sino devuelve nil
(defun esVector (var mem)
	(if (null mem) nil
		(if (equal (car (car mem)) var) 
			(if (listp (car (cdr (car mem))))	T
				(esVector var (cdr mem))
			)
		)
	)
)


;Si es una variable o es un ++ o un -- se entiende que la expresion es una asignacion de variables
(defun esAsignacion (expr memoria)
	(if (existeVariable (car expr) memoria) T
		(or (equal (car expr) '++) (equal (car expr) '--))
	)
)


;Procesa una expresion C que es una asignacion y la guarda en memoria.
(defun asignacion (expr memoria)
	(if (existeVariable (car expr) memoria) 
		(cond
			( (and (equal (nth 1 expr) '=) (eq (length expr)  3)) (asociar (car expr) (nth 2 expr) memoria) )
			( (equal (nth 1 expr) '=) (asociar (car expr) (evaluar (cdr (cdr expr)) memoria (nth 2 expr)) memoria))
			;Caso de asignacion de vector 
			( (equal (nth 4 expr) '=) (asociar (car expr) (evaluar (cdr (cdr (cdr (cdr (cdr expr))))) memoria) memoria (nth 2 expr)))
			( (and (equal (nth 4 expr) '++) (eq (length expr) 5)) (asignacion (list (nth 0 expr)(nth 1 expr)(nth 2 expr)(nth 3 expr) '= 
																											     (nth 0 expr)(nth 1 expr)(nth 2 expr)(nth 3 expr) '+ 1 ) memoria))
			
			( (equal (nth 1 expr) '++) (asignacion (list (car expr) '= (car expr) '+ 1 ) memoria))
			( (equal (nth 1 expr) '--) (asignacion (list (car expr) '= (car expr) '- 1 ) memoria))
			( (and (equal (nth 1 expr) '-=) (eq (length expr)  3)) (asignacion (list (car expr) '=  (car expr) '- (nth 2 expr)) memoria) ) 
			( (equal (nth 1 expr) '-=)  (asignacion (list (car expr) '=  (car expr) '- (evaluar (cdr (cdr expr)) memoria)) memoria) ) 
			( (and (equal (nth 1 expr) '+=) (eq (length expr)  3)) (asignacion (list (car expr) '=  (car expr) '+ (nth 2 expr)) memoria) )
			( (equal (nth 1 expr) '+=) (asignacion (list (car expr) '=  (car expr) '+ (evaluar (cdr (cdr expr)) memoria)) memoria) ) 
			( (and (equal (nth 1 expr) '*=) (eq (length expr)  3)) (asignacion (list (car expr) '=  (car expr) '* (nth 2 expr)) memoria) ) 
			( (equal (nth 1 expr) '*=) (asignacion (list (car expr) '=  (car expr) '* (evaluar (cdr (cdr expr)) memoria)) memoria) )
			( T (asignacion (list (car l) '= (car l) (nth 1 expr) (nth 3 expr)) memoria))
		)
		(asignacion (reverse expr) memoria)
	)
)


;Procesa  una instruccion if
(defun procesar_if (prg ent mem salida)
	;Evaluo la condicion
	(if (not (equal (evaluar (car (cdr (car prg))) mem) 0))
		;Ejecuto if si la condicion es verdadera
		(ejecutar (append (nth 2 (car prg)) (cdr prg)) ent mem salida)
		;Si la condicion es falsa chequeo si tiene else sino sigo ejecutando la siguiente instruccion
		(if (equal (length (car prg)) 5) (ejecutar (append (nth 4 (car prg)) (cdr prg)) ent mem salida)
			(ejecutar (cdr prg) ent mem salida)
		) 
	)
)


;Proceso un ciclo while 
(defun procesar_while (prg ent mem salida)
	(if (not (equal (evaluar (nth 1 (car prg)) mem) 0))
		(ejecutar (append (nth 2 (car prg) ) prg) ent mem salida)
		(ejecutar (cdr prg) ent mem salida)
	)
)
