; Estrutura - Suguru
(defstruct suguru board regions)

; Funções auxiliares:
; ----------------------------------------------------------------------------------------------------------------------
; Cria uma lista de 1 à n 
(defun l (n) (loop :for x :below n :collect (+ 1 x)))
; Cria um par com os valores indicados
(defun p (x y) (cons x y))
; Separa uma lista em uma lista de listas
(defun group (l n) 
  (when l 
    (cons (subseq l 0 (min n (length l))) 
      (group (nthcdr n l) n))))
; Transforma uma lista 2D, ou mais, em uma lista 1D
(defun flatten (l)
  (cond ((null l) nil)
        ((atom l) (list l))
        (t (loop for a in l appending (flatten a))))
)

; Variáveis globais:
; ----------------------------------------------------------------------------------------------------------------------
(defvar *indexes*) ; Indexes do Suguru a ser resolvido
(defvar *lengthX*) ; Quantidade de linhas do Suguru
(defvar *lengthY*) ; Quantidade de colunas do Suguru

; Exemplos de Suguru:
; ----------------------------------------------------------------------------------------------------------------------
; Exemplo de suguru simples 5 x 5, com 6 regiões
(defvar suguru_1 
  (make-suguru
    ; Células
    :board (list
      ; Linha 0
      (list (l 4) (l 4) (l 4) (l 5) (l 5))
      ; Linha 1
      (list (l 4) (l 5) (l 5) (l 5) (l 5))
      ; Linha 2
      (list (l 5) (l 5) (l 5) (l 5) (l 5))
      ; Linha 3
      (list (l 5) (l 5) (l 5) (l 5) (l 5))
      ; Linha 4
      (list (l 5) (l 5) (l 5) (l 5) (l 1))
    )

    ; Regiões
    :regions (list
      ; Região 0
      (list (p 0 0) (p 0 1) (p 0 2) (p 1 0))
      ; Região 1
      (list (p 0 3) (p 0 4) (p 1 4) (p 2 4) (p 3 4))
      ; Região 2
      (list (p 1 1) (p 1 2) (p 2 0) (p 2 1) (p 3 0))
      ; Região 3
      (list (p 1 3) (p 2 2) (p 2 3) (p 3 1) (p 3 2))
      ; Região 4
      (list (p 3 3) (p 4 0) (p 4 1) (p 4 2) (p 4 3))
      ; Região 5
      (list (p 4 4))
    )
  )
)

; Exemplo de suguru 6 x 6, com 8 regiões (Suguru Nº 1)
(defvar suguru_2
  (make-suguru
    ; Células
    :board (list
      ; Linha 0
      (list 4     (l 4) (l 5) (l 5) (l 5) (l 5))
      ; Linha 1
      (list (l 4) (l 5) (l 5) (l 5) (l 5) (l 5))
      ; Linha 2
      (list (l 4) (l 5) 4     (l 5) (l 5) 1    )
      ; Linha 3
      (list (l 5) (l 5) (l 5) 2     (l 5) (l 5))
      ; Linha 4
      (list 5     (l 2) (l 2) 3     5     (l 5))
      ; Linha 5
      (list (l 5) (l 5) (l 5) (l 5) (l 5) (l 5))
    )

    ; Regiões
    :regions (list
      ; Região 0
      (list (p 0 0) (p 0 1) (p 1 0) (p 2 0))
      ; Região 1
      (list (p 0 2) (p 1 1) (p 1 2) (p 1 3) (p 2 2))
      ; Região 2
      (list (p 0 3) (p 0 4) (p 0 5) (p 1 4) (p 2 4))
      ; Região 3
      (list (p 1 5) (p 2 5) (p 3 5) (p 4 5) (p 5 5))
      ; Região 4
      (list (p 2 1) (p 3 0) (p 3 1) (p 4 0) (p 5 0))
      ; Região 5
      (list (p 2 3) (p 3 2) (p 3 3) (p 3 4) (p 4 3))
      ; Região 6
      (list (p 4 1) (p 4 2))
      ; Região 7
      (list (p 4 4) (p 5 1) (p 5 2) (p 5 3) (p 5 4))
    )
  )
)

; Exemplo de suguru 8 x 8, com 15 regiões (Suguru Nº 12)
(defvar suguru_3
  (make-suguru
    ; Células
    :board (list
      ; Linha 0
      (list 2     (l 1) (l 5) (l 5) (l 5) (l 5) (l 5) (l 5))
      ; Linha 1
      (list (l 5) (l 5) (l 5) (l 5) (l 5) 5     (l 5) 1    )
      ; Linha 2
      (list (l 5) (l 5) (l 5) (l 5) (l 5) (l 5) (l 5) (l 5))
      ; Linha 3
      (list (l 5) 3     5     (l 5) (l 5) (l 5) (l 5) (l 5))
      ; Linha 4
      (list (l 4) (l 4) (l 3) (l 3) (l 5) 4     (l 5) (l 5))
      ; Linha 5
      (list (l 4) 5     (l 3) 5     (l 5) (l 5) (l 5) 3    )
      ; Linha 6
      (list (l 5) 4     (l 5) 3     (l 5) (l 5) 2     5    )
      ; Linha 7
      (list (l 1) (l 5) (l 5) (l 5) (l 5) (l 5) (l 5) (l 5))
    )

    ; Regiões
    :regions (list
      ; Região 0
      (list (p 0 0) (p 1 0) (p 2 0) (p 2 1) (p 3 0))
      ; Região 1
      (list (p 0 1))
      ; Região 2
      (list (p 0 2) (p 1 1) (p 1 2) (p 1 3) (p 2 2))
      ; Região 3
      (list (p 0 3) (p 0 4) (p 0 5) (p 0 6) (p 1 4))
      ; Região 4
      (list (p 0 7) (p 1 6) (p 1 7) (p 2 7) (p 3 7))
      ; Região 5
      (list (p 1 5) (p 2 4) (p 2 5) (p 2 6) (p 3 5))
      ; Região 6
      (list (p 2 3) (p 3 2) (p 3 3) (p 3 4) (p 4 4))
      ; Região 7
      (list (p 3 1) (p 4 0) (p 4 1) (p 5 0))
      ; Região 8
      (list (p 3 6) (p 4 6) (p 4 7) (p 5 7) (p 6 7))
      ; Região 9
      (list (p 4 2) (p 4 3) (p 5 2))
      ; Região 10
      (list (p 4 5) (p 5 4) (p 5 5) (p 5 6) (p 6 5))
      ; Região 11
      (list (p 5 1) (p 6 0) (p 6 1) (p 6 2) (p 7 1))
      ; Região 12
      (list (p 5 3) (p 6 3) (p 6 4) (p 7 2) (p 7 3))
      ; Região 13
      (list (p 6 6) (p 7 4) (p 7 5) (p 7 6) (p 7 7))
      ; Região 14
      (list (p 7 0))
    )
  )
)

; Exemplo de suguru 10 x 10, com 18 regiões (Suguru Nº 160)
(defvar suguru_4
  (make-suguru
    ; Células
    :board (list
      ; Linha 0
      (list 6     2     5     3     (l 7) 7     (l 7) 7     2     (l 5))
      ; Linha 1
      (list 5     4     (l 7) 2     1     (l 7) 3     (l 7) (l 5) 1    )
      ; Linha 2
      (list 3     (l 6) 3     (l 7) 5     (l 7) 2     (l 7) (l 5) 7    )
      ; Linha 3
      (list (l 6) 4     2     6     (l 7) (l 7) (l 7) 4     (l 5) 4    )
      ; Linha 4
      (list 1     5     (l 7) (l 7) 1     4     (l 6) 2     6     2    )
      ; Linha 5
      (list 7     (l 7) 6     (l 7) 3     (l 6) 3     (l 1) 3     (l 7))
      ; Linha 6
      (list (l 1) 4     (l 6) (l 6) (l 6) 5     2     (l 5) (l 5) (l 5))
      ; Linha 7
      (list (l 6) (l 6) (l 7) 7     2     (l 7) (l 7) 5     (l 5) 5    )
      ; Linha 8
      (list (l 7) (l 7) 4     (l 7) 4     7     (l 7) (l 7) 6     (l 5))
      ; Linha 9
      (list 5     (l 2) (l 2) (l 7) 6     (l 7) 5     (l 5) 3     2    )
    )

    ; Regiões
    :regions (list
      ; Região 0
      (list (p 0 0) (p 1 0) (p 2 0) (p 2 1) (p 3 0) (p 3 1))
      ; Região 1
      (list (p 0 1) (p 0 2) (p 0 3) (p 0 4) (p 0 5) (p 0 6) (p 1 1))
      ; Região 2
      (list (p 0 7) (p 1 2) (p 1 3) (p 1 4) (p 1 5) (p 1 6) (p 1 7))
      ; Região 3
      (list (p 0 8) (p 0 9) (p 1 8) (p 2 8) (p 3 8))
      ; Região 4
      (list (p 1 9) (p 2 9) (p 3 9) (p 4 8) (p 4 9) (p 5 8) (p 5 9))
      ; Região 5
      (list (p 2 2) (p 2 3) (p 3 2) (p 3 3) (p 3 4) (p 4 2) (p 4 3))
      ; Região 6
      (list (p 2 4) (p 2 5) (p 2 6) (p 2 7) (p 3 5) (p 3 6) (p 3 7))
      ; Região 7
      (list (p 4 0) (p 4 1) (p 5 0) (p 5 1) (p 5 2) (p 5 3) (p 5 4))
      ; Região 8
      (list (p 4 4) (p 4 5) (p 4 6) (p 4 7) (p 5 5) (p 5 6))
      ; Região 9
      (list (p 5 7)) 
      ; Região 10
      (list (p 6 0))
      ; Região 11
      (list (p 6 1) (p 6 2) (p 6 3) (p 6 4) (p 7 0) (p 7 1))
      ; Região 12
      (list (p 6 5) (p 6 6) (p 7 5) (p 7 6) (p 8 6) (p 8 7) (p 8 8))
      ; Região 13
      (list (p 6 7) (p 6 8) (p 6 9) (p 7 7) (p 7 8))
      ; Região 14
      (list (p 7 2) (p 7 3) (p 7 4) (p 8 0) (p 8 1) (p 8 2) (p 9 0))
      ; Região 15
      (list (p 7 9) (p 8 9) (p 9 7) (p 9 8) (p 9 9))
      ; Região 16
      (list (p 8 3) (p 8 4) (p 8 5) (p 9 3) (p 9 4) (p 9 5) (p 9 6))
      ; Região 17
      (list (p 9 1) (p 9 2))
    )
  )
)

; Exemplo de suguru vazio 6 x 6, com 8 regiões e uso de backtracking (Suguru Nº 1)
(defvar suguru_5
  (make-suguru
    ; Células
    :board (list
      ; Linha 0
      (list (l 4) (l 4) (l 5) (l 5) (l 5) (l 5))
      ; Linha 1
      (list (l 4) (l 5) (l 5) (l 5) (l 5) (l 5))
      ; Linha 2
      (list (l 4) (l 5) (l 5) (l 5) (l 5) (l 5))
      ; Linha 3
      (list (l 5) (l 5) (l 5) (l 5) (l 5) (l 5))
      ; Linha 4
      (list (l 5) (l 2) (l 2) (l 5) (l 5) (l 5))
      ; Linha 5
      (list (l 5) (l 5) (l 5) (l 5) (l 5) (l 5))
    )

    ; Regiões
    :regions (list
      ; Região 0
      (list (p 0 0) (p 0 1) (p 1 0) (p 2 0))
      ; Região 1
      (list (p 0 2) (p 1 1) (p 1 2) (p 1 3) (p 2 2))
      ; Região 2
      (list (p 0 3) (p 0 4) (p 0 5) (p 1 4) (p 2 4))
      ; Região 3
      (list (p 1 5) (p 2 5) (p 3 5) (p 4 5) (p 5 5))
      ; Região 4
      (list (p 2 1) (p 3 0) (p 3 1) (p 4 0) (p 5 0))
      ; Região 5
      (list (p 2 3) (p 3 2) (p 3 3) (p 3 4) (p 4 3))
      ; Região 6
      (list (p 4 1) (p 4 2))
      ; Região 7
      (list (p 4 4) (p 5 1) (p 5 2) (p 5 3) (p 5 4))
    )
  )
)

; Exemplo de suguru vazio 7 x 6, com 8 regiões e uso de backtracking
(defvar suguru_6
  (make-suguru
    ; Células
    :board (list
      ; Linha 0
      (list (l 4) (l 4) (l 5) (l 5) (l 5) (l 5))
      ; Linha 1
      (list (l 4) (l 5) (l 5) (l 5) (l 5) (l 6))
      ; Linha 2
      (list (l 4) (l 6) (l 5) (l 5) (l 5) (l 6))
      ; Linha 3
      (list (l 6) (l 6) (l 5) (l 5) (l 5) (l 6))
      ; Linha 4
      (list (l 6) (l 2) (l 2) (l 5) (l 9) (l 6))
      ; Linha 5
      (list (l 6) (l 9) (l 9) (l 9) (l 9) (l 6))
      ; Linha 6
      (list (l 6) (l 9) (l 9) (l 9) (l 9) (l 6))
    )

    ; Regiões
    :regions (list
      ; Região 0
      (list (p 0 0) (p 0 1) (p 1 0) (p 2 0))
      ; Região 1
      (list (p 0 2) (p 1 1) (p 1 2) (p 1 3) (p 2 2))
      ; Região 2
      (list (p 0 3) (p 0 4) (p 0 5) (p 1 4) (p 2 4))
      ; Região 3
      (list (p 1 5) (p 2 5) (p 3 5) (p 4 5) (p 5 5) (p 6 5))
      ; Região 4
      (list (p 2 1) (p 3 0) (p 3 1) (p 4 0) (p 5 0) (p 6 0))
      ; Região 5
      (list (p 2 3) (p 3 2) (p 3 3) (p 3 4) (p 4 3))
      ; Região 6
      (list (p 4 1) (p 4 2))
      ; Região 7
      (list (p 4 4) (p 5 1) (p 5 2) (p 5 3) (p 5 4) (p 6 1) (p 6 2) (p 6 3) (p 6 4))
    )
  )
)

; Exemplo de suguru inválido 6 x 6, com 8 regiões (Suguru Nº 1)
(defvar suguru_7
  (make-suguru
    ; Células
    :board (list
      ; Linha 0
      (list (l 4) (l 4) (l 5) (l 5) (l 5) (l 5))
      ; Linha 1
      (list (l 4) (l 5) (l 5) (l 5) (l 5) (l 5))
      ; Linha 2
      (list (l 4) (l 5) (l 5) (l 5) (l 5) (l 5))
      ; Linha 3
      (list (l 5) (l 5) (l 5) (l 5) (l 5) (l 5))
      ; Linha 4
      (list (l 5) 1     1     (l 5) (l 5) (l 5))
      ; Linha 5
      (list (l 5) (l 5) (l 5) (l 5) (l 5) (l 5))
    )

    ; Regiões
    :regions (list
      ; Região 0
      (list (p 0 0) (p 0 1) (p 1 0) (p 2 0))
      ; Região 1
      (list (p 0 2) (p 1 1) (p 1 2) (p 1 3) (p 2 2))
      ; Região 2
      (list (p 0 3) (p 0 4) (p 0 5) (p 1 4) (p 2 4))
      ; Região 3
      (list (p 1 5) (p 2 5) (p 3 5) (p 4 5) (p 5 5))
      ; Região 4
      (list (p 2 1) (p 3 0) (p 3 1) (p 4 0) (p 5 0))
      ; Região 5
      (list (p 2 3) (p 3 2) (p 3 3) (p 3 4) (p 4 3))
      ; Região 6
      (list (p 4 1) (p 4 2))
      ; Região 7
      (list (p 4 4) (p 5 1) (p 5 2) (p 5 3) (p 5 4))
    )
  )
)

; Helpers:
; ----------------------------------------------------------------------------------------------------------------------
; Imprime o Suguru formatado
(defun showSuguru (suguru)
  (cond
    ((null suguru) (format t "Suguru sem resposta"))
    (T (dolist (line (suguru-board suguru)) (format t "~{~a ~^~}~%" line)))
  )

  (terpri)
)

; Inicializa as variáveis globais com as informações do Suguru a ser solvido
(defun indexes (suguru)
  (setf *indexes* ())
  (setf *lengthX* (length (suguru-board suguru)))
  (setf *lengthY* (length (elt (suguru-board suguru) 0)))
  
  (dotimes (i *lengthX*)
    (dotimes (j *lengthY*)
      (push (p i j) *indexes*)
    )
  )
  
  (setf *indexes* (reverse *indexes*))
)

; Verificações:
; ------------------------------------------------------------------------------
; Verifica se o Suguru está solucionado
(defun isSolved (suguru)
  (every #'(lambda (x) x) (mapcar (lambda (line) (notany #'listp line)) (suguru-board suguru)))
)

; Verifica se o tabuleiro atual tem solução
(defun hasSolution (suguru)
  (every #'(lambda (x) x) (mapcar (lambda (line) (not (member nil line))) (suguru-board suguru)))
)

; Getters:
; ------------------------------------------------------------------------------
; Retorna um conjunto de células, a partir de um conjunto de posições
(defun getCell (positions board)
  (loop
    for pos in positions
    for x = (car pos)
    for y = (cdr pos)
    collect (elt (elt board x) y)
  )
)

; Retorna os valores fixos de um conjunto de células
(defun getFixed (cells)
  (remove-if-not #'(lambda (cell) (not (listp cell))) cells)
)

; Retorna as células adjacentes de determinada posição
(defun getAdj (pos board)
  (let ((x (car pos))
        (y (cdr pos)))
    (getCell
      (remove-if
        #'(lambda (pos) (or (< (car pos) 0) (< (cdr pos) 0) (>= (car pos) *lengthX*) (>= (cdr pos) *lengthY*)))
          (list 
            (cons (- x 1) y)
            (cons (+ x 1) y)
            (cons x (- y 1))
            (cons x (+ y 1))
            (cons (+ x 1) (- y 1))
            (cons (+ x 1) (+ y 1))
            (cons (- x 1) (- y 1))
            (cons (- x 1) (+ y 1))
          )
      )
      board
    )
  )
)

; Retorna todas as células de uma região, exceto a célula da posição inicial informada
(defun getRegion (pos suguru)
  (getCell
    (remove-if #'(lambda (x) (equal x pos))
      (car 
        (loop for region in (suguru-regions suguru)
          if (member pos region :test #'equal)
            collect region
        )
      )
    )
    (suguru-board suguru)
  )
)

; Setters:
; ------------------------------------------------------------------------------
; Fixa uma célula, caso ela possua apenas uma possibilidade
(defun fixCell (cell)
  (if (and (listp cell) (= (length cell) 1))
    (car cell)
    cell
  )
)

; Fixa uma célula, caso ela possua uma possibilidade única dentre os valores indicados
(defun fixUnique (cell values)
  (if 
    (and 
      (listp cell) 
      (= (length (set-difference cell values :test 'equal)) 1)
    )

    (car (set-difference cell values :test 'equal))
    cell
  )
)

; Remove, das possibilidades de uma célula, um conjunto de números especificado
; Realiza verificações quanto a solucionabilidade do tabuleiro
(defun trimCell (cell values)
  (if (listp cell)
    (set-difference cell values)
    (if (member cell values) NIL cell)
  )
)

; Solucionador linear:
; ----------------------------------------------------------------------------------------------------------------------
; Percorre o Suguru e remove das possibilidades de células ainda não fixadas, os números fixados, de sua adjacência
(defun trimAdj (suguru)
  (group 
    (loop
      for pos in *indexes*
      for x = (car pos)
      for y = (cdr pos)
      collect
        (trimCell (elt (elt (suguru-board suguru) x) y) (getFixed (getAdj pos (suguru-board suguru))))  
    )
  *lengthY*)
)

; Percorre o Suguru e remove das possibilidades de células ainda não fixadas, os números fixados, de sua região
(defun trimRegion (suguru)
  (group
    (loop
      for pos in *indexes*
      for x = (car pos)
      for y = (cdr pos)
      collect
        (trimCell (elt (elt (suguru-board suguru) x) y) (getFixed (getRegion pos suguru)))  
    )
  *lengthY*)
)

; Percorre o Suguru e fixa as células que, em sua região, possuam possibilidades únicas
(defun checkUnique (suguru)
  (group
    (loop
      for pos in *indexes*
      for x = (car pos)
      for y = (cdr pos)
      collect
        (fixUnique (elt (elt (suguru-board suguru) x) y) (flatten (getRegion pos suguru)))  
    )
  *lengthY*)
)

; Percorre o Suguru e fixa as células que possuem apenas uma possibilidade
(defun fixIt (suguru)
  (group
    (loop
      for pos in *indexes*
      for x = (car pos)
      for y = (cdr pos)
      collect
        (fixCell (elt (elt (suguru-board suguru) x) y))  
    )
  *lengthY*)
)

; Aplica as funções anteriores repetidamente e tenta resolver o Suguru
; Retorna (T . Suguru), caso consiga resolver
; Retorna (NIL . Suguru), caso fique preso em loop e não consiga resolver
; Retorna (NIL . NIL), caso o tabuleiro não possua solução
(defun solveSimple (suguru)
  (let ((counter (length (flatten (suguru-board suguru)))))
    (setf (suguru-board suguru) (checkUnique suguru))
    (setf (suguru-board suguru) (fixIt suguru))
    (setf (suguru-board suguru) (trimAdj suguru))
    (setf (suguru-board suguru) (trimRegion suguru))

    (cond
      ((isSolved suguru) (cons T suguru))
      ((not (hasSolution suguru)) (cons NIL NIL))
      ((= counter (length (flatten (suguru-board suguru)))) (cons NIL suguru))
      (T (solveSimple suguru))
    )
  )
)

; Backtracking
; ----------------------------------------------------------------------------------------------------------------------
; Retorna a primeira célula não fixada de um suguru e sua posição
(defun getPossible (suguru)
  (cond
    ((null suguru) (cons 0 (p 0 0)))
    (T (car
      (loop
        for pos in *indexes*
        for x = (car pos)
        for y = (cdr pos)
        if (listp (elt (elt (suguru-board suguru) x) y))
        collect
          (cons (elt (elt (suguru-board suguru) x) y) (p x y)) 
      )
    ))
  )
)

; Cria um novo suguru com uma célula inserida em um lugar especificado
(defun tryWith (posC cell suguru)
  (make-suguru
    :board
      (group
        (loop
          for pos in *indexes*
          for x = (car pos)
          for y = (cdr pos)
          collect
            (if (equal pos posC)
              cell
              (elt (elt (suguru-board suguru) x) y)
            )
        )
      *lengthY*)
    
    :regions
      (suguru-regions suguru)
  )
)

; Tenta resolver o suguru, fixando possibilidades e aplicando o solucionador linear simples
(defun backtrackSolve (posC cell suguru)
  (cond
    ((null suguru) NIL)
    ((null cell) NIL)
    (T (let ((newSuguru (solve (tryWith posC (car cell) suguru))))
      (if (null newSuguru)
        (backtrackSolve posC (cdr cell) suguru)
        newSuguru
      )
    ))
  )
)

; Solucionador final:
; ----------------------------------------------------------------------------------------------------------------------
; Resolve o Suguru
(defun solve (suguru)
  (indexes suguru)

  (let ((newSuguru (solveSimple suguru)))
  (let ((sign (car newSuguru))
        (board (cdr newSuguru)))
  (let ((possible (getPossible board)))
    (if (not sign)
      (backtrackSolve (cdr possible) (car possible) board)
      board
    )
  )))
)
; ----------------------------------------------------------------------------------------------------------------------

(defun main ()
  (showSuguru (solve suguru_1))
  (showSuguru (solve suguru_2))
  (showSuguru (solve suguru_3))
  (showSuguru (solve suguru_4))
  (showSuguru (solve suguru_5))
  (showSuguru (solve suguru_6))
  (showSuguru (solve suguru_7))
)

(main)