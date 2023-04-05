;apos alterar os dois tabuleiros e n no código fonte, digite clisp suguru.lisp no terminal que a solucao sera impressa na tela

;O tabuleiro que queremos resolver, zeros representam espaços vazios                                  --ALTERAR VALOR AQUI
(setf tabuleiro (list    0  3  0  2  0  3 
                         4  0  4  0  1  0 
                         0  0  0  5  0  4
                         3  5  0  0  0  0
                         0  0  0  2  0  0
                         5  2  0  0  4  0))

; O tabuleiro com as areas definidas atraves de tuplas                                                 --ALTERAR VALOR AQUI
(setf tabuleiroDiv (list    '(1 0) '(1 3) '(1 0) '(2 2) '(2 0) '(2 3)
                            '(3 4) '(4 0) '(1 4) '(5 0) '(2 1) '(2 0)
                            '(3 0) '(4 0) '(4 0) '(5 5) '(5 0) '(5 4)
                            '(3 3) '(3 5) '(4 0) '(6 0) '(6 0) '(5 0)
                            '(7 0) '(3 0) '(4 0) '(6 2) '(6 0) '(8 0)
                            '(7 5) '(7 2) '(7 0) '(7 0) '(6 4) '(8 0)))


;--2 3 1 2 4 3
;--4 5 4 3 1 5
;--2 1 2 5 2 4               <----Solucao para referencia
;--3 5 3 1 3 1
;--4 1 4 2 5 2
;--5 2 3 1 4 1


;n é a dimensao do tabuleiro(tabuleiro tera tamanho nxn)                                               --ALTERAR VALOR AQUI
(setq n 6)

;g é o index do ultimo elemento do tabuleiro
(setq g (- (* n n) 1))

;matriz com os digitos a serem testados
(setf possibilities (list 1 2 3 4 5 6 7 8 9))


;----------------------------------------------------------------------------Funcoes para testar os lados e diagonais em volta do elemento--------------------------------------------

;--testa se o quadrado a esquerda tem um número igual
(defun leftOk(i lista)
    (if (= (mod i n) 0) T
        (if (= (nth (- i 1) lista) 0) T
            (if (= (nth i lista) (nth (- i 1) lista))
                NIL
                T)))
)

;--testa se o quadrado a direita tem um número igual
(defun rightOk(i lista)
    (if (= (mod (+ i 1) n) 0) T
        (if (= (nth (+ i 1) lista) 0) T
            (if (= (nth i lista) (nth (+ i 1) lista))
                NIL
                T)))
)

;--testa se o quadrado acima tem um número igual
(defun upOk(i lista)
    (if (< i n) T
        (if (= (nth (- i n) lista) 0) T
            (if (= (nth i lista) (nth (- i n) lista))
                NIL
                T)))
)

;--testa se o quadrado abaixo tem um número igual
(defun downOk(i lista)
    (if (>= i (* n (- n 1))) T
        (if (= (nth (+ i n) lista) 0) T
            (if (= (nth i lista) (nth (+ i n) lista))
                NIL
                T)))
)

;--testa se o quadrado ao nordeste tem um número igual
(defun neOk(i lista)
    (if (or (< i n) (= (mod (+ i 1) n) 0)) T
        (if (= (nth (- (+ i 1) n) lista) 0) T
            (if (= (nth i lista) (nth (- (+ i 1) n) lista))
                NIL
                T)))
)

;--testa se o quadrado ao noroeste tem um número igual
(defun nwOk(i lista)
    (if (or (< i n) (= (mod i n) 0)) T
        (if (= (nth (- (- i 1) n) lista) 0) T
            (if (= (nth i lista) (nth (- (- i 1) n) lista))
                NIL
                T)))
)

;--testa se o quadrado ao sudeste tem um número igual
(defun seOk(i lista)
    (if (or (>= i (* n (- n 1))) (= (mod (+ i 1) n) 0)) T
        (if (= (nth (+ (+ i 1) n) lista) 0) T
            (if (= (nth i lista) (nth (+ (+ i 1) n) lista))
                NIL
                T)))
)

;--testa se o quadrado ao sudoeste tem um número igual
(defun swOk(i lista)
    (if (or (>= i (* n (- n 1))) (= (mod i n) 0)) T
        (if (= (nth (- (+ i n) 1) lista) 0) T
            (if (= (nth i lista) (nth (- (+ i n) 1) lista))
                NIL
                T)))
)

;--testa se algum dos quadrados em volta tem um número igual
(defun sidesOk(i lista)
    (if (and (rightOk i lista) (leftOk i lista) (upOk i lista) (downOk i lista) (neOk i lista) (nwOk i lista) (seOk i lista) (swOk i lista)) T NIL)
)


;----------------------------------------------------------------------------Funcoes para testar os números na mesma area do elemento-------------------------------------------------


;--testa se duas tuplas são iguais e retorna 1 se sim e 0 se não
(defun isEqual(x y)
    (if (and (= (nth 0 x) (nth 0 y)) (= (nth 1 x) (nth 1 y)))
        1
        0)
)

;--soma os elementos de uma lista
(defun somaLista (lista)
    (if lista
      (+ (car lista) (somaLista (cdr lista)))
      0)
)

;--testa se tem dois números iguais na área do indice i
(defun areaOk(i lista)
    (if (= (somaLista (mapcar (lambda (x) (isEqual x (nth i lista))) lista)) 2)
        NIL
        T)
)


;------------------------------------------------------------------Funcoes para testar se o elemento esta no intervalo de possibilidades para aquela area-----------------------------

;--testa se duas tuplas tem o mesmo primeiro elemento
(defun isEqualp (x y)
    (if (= (nth 0 x) (nth 0 y))
        1
        0)
)

;--retorna quantos quadrados tem a área do indice i
(defun areaSize(i lista)
    (somaLista (mapcar (lambda (x) (isEqualp x (nth i lista))) lista))
)

;--testa se o elemento e é maior do que a quantidade de quadrados da área do indice i
(defun possibilitiesOk(i lista)
    (if (< (nth 1 (nth i lista)) (+ (areaSize i lista) 1))
        T
        NIL)
)


;-------------------------------------------------------------------------------------Funcoes para encontrar a solucao----------------------------------------------------------------

;--aplica todos os testes previamente implementados
(defun tester(i lista listaDiv)
    (and (sidesOk i lista) (areaOk i listaDiv) (possibilitiesOk i listaDiv))
)

;--testa se o tabuleiro foi completamente preenchido
(defun isEnd(i lista)
    (if (and (= i g) (/= (nth i lista) 0)) 
        T
        NIL)
)

;--encontra o próximo espaço vazio na matriz
(defun nextBlank(i lista)
    (cond   ((= i g) g)
            ((= (nth (+ i 1) lista) 0) (+ i 1))
            (T (nextBlank (+ i 1) lista)))
)

;--retorna um novo tabuleiro com o elemento x inserido no indice i
(defun getTesterMap(x i lista)
    (concatenate 'list (concatenate 'list (subseq lista 0 i) (list x)) (subseq lista (+ i 1)))
)

;--retorna um novo tabuleiroDiv com o elemento x inserido no indice i
(defun getTesterDivMap(x i listaDiv)
    
    (concatenate 'list (concatenate 'list (subseq listaDiv 0 i) (list (list (nth 0 (nth i listaDiv)) x)) (subseq listaDiv (+ i 1))))
)

;--aplica a função solver com todos os elementos da matriz x(possibilities)
(defun mapT(i lista listaDiv)
    (mapcar (lambda (x) (solver x i lista listaDiv)) possibilities)
)

;--funcao recursiva que encontra a solução do suguru
(defun solver(x i lista listaDiv)
    (if (tester i (getTesterMap x i lista) (getTesterDivMap x i listaDiv))
        (if (isEnd i (getTesterMap x i lista))
            (getTesterMap x i lista)
            (mapT (nextBlank i lista) (getTesterMap x i lista) (getTesterDivMap x i listaDiv)))
        NIL
    )
)


;-------------------------------------------------------------------------------------Funcoes para printar a solucao------------------------------------------------------------------

;imprime o resultado na tela
(defun main()
    (write-line (write-to-string (mapT 0 tabuleiro tabuleiroDiv)))
)

(main)