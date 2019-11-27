;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname lab5-sem-contract) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "abstraction.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t write repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "abstraction.rkt" "teachpack" "2htdp")) #f)))
(define-struct predio (altura base pos material impactos))
;; uma estrutura do tipo predio tem o seguinte formato:
;; (make-predio a b x m i), onde:
;; a: Número, altura do prédio
;; b: Número, base do prédio
;; x: Número, posicao no eixo x do prédio
;; m: Número entre 1 e 3, material do prédio, onde 1 é o mais fŕagil, e 3 o mais resistente
;; i: Número, número de impactos que o pŕedio pode receber
;; constantes:
(define PREDIO1 (make-predio 50 20 50 1 5))
(define PREDIO2 (make-predio 70 20 100 2 0))
(define PREDIO3 (make-predio 90 30 200 3 0))

(define CENA-LARGURA 500)
(define CENA-ALTURA 400)
(define COR-MATERIAL1 "DarkMagenta")
(define COR-MATEIRAL2 "Pink")
(define COR-MATERIAL3 "DarkBlue")
(define COR-METEORO1 "Brown")
(define COR-METEORO2 "DarkGrey")
(define COR-METEORO3 "White")
(define CENA-INICIAL(empty-scene CENA-LARGURA CENA-ALTURA "LightSkyBlue"))
(define NUVEM (ellipse 140 50 "solid" "white"))
(define SOL (circle  30 "solid" "yellow"))
(define CHAO (rectangle 1000 50 "solid" "black"))
(define CENA (place-image SOL 400 50 (place-image CHAO 0 375 CENA-INICIAL)))

;; uma lista-de-predios é:
;; 1. empty, ou
;; 2. (cons p lp), onde
;; p = predio
;; lp = lista-de-predios
;; que obedece o predicado de (listof predio?)
(define LISTAP1 (list PREDIO1 PREDIO2 PREDIO3))

;; inicializa-lista-predios: lista-de-predios -> lista-de-predios
;; inicializa os impactos de uma lista de prédios com base no material
(define (inicializa-lista-predios listap)
  (map inicializa-predio listap)) ;; mapeamento de função para uma lista
;; testes:


;; inicializa-predio: predios -> predios
;; iniciliza um predio, dado que o material define os impactos do prédio
;; ex:
;; (inicializa-predio PREDIO1) = (make-predio 50 20 50 1 1)
;; (inicializa-predio PREDIO3) = (make-predio 90 30 200 3 5)
(define (inicializa-predio p)
  (cond
    [(= (predio-material p) 1)
     (make-predio
      (predio-altura p)
      (predio-base p)
      (predio-pos p)
      (predio-material p)
      1)]
    [(= (predio-material p) 2)
     (make-predio
      (predio-altura p)
      (predio-base p)
      (predio-pos p)
      (predio-material p)
      3)]
    [(= (predio-material p) 3)
     (make-predio
      (predio-altura p)
      (predio-base p)
      (predio-pos p)
      (predio-material p)
      5)]
    [else (error "prédio tem um material de tipo inváldio (1,2 ou 3)")]))
;; testes:
(check-expect (inicializa-predio PREDIO1) (make-predio 50 20 50 1 1))
(check-expect (inicializa-predio PREDIO3) (make-predio 90 30 200 3 5))

(define LISTAP1I (inicializa-lista-predios LISTAP1))
(define POSICAO-Y-PREDIOS 330)
;; coloca-predio: predio Imagem -> Imagem
;; desenha o prédio na cena fornecida
;; ex: (coloca-predio PREDIO1 CENA) =
;;
(define (coloca-predio predio cena)
  (place-image (desenha-predio predio) (predio-pos predio) POSICAO-Y-PREDIOS cena))
;; testes

;; desenha-predio: predio -> Imagem
(define (desenha-predio predio)
  (overlay (desenha-contorno predio)
  (rectangle (predio-base predio) (predio-altura predio) "solid" (escolhe-cor predio))))
;; testes:

     
;; escolhe-cor: predio -> String
;; dado um predio, devolve a cor do material
(define (escolhe-cor pr)
  (cond
    ([= (predio-material pr) 1]
     COR-MATERIAL1)
    ([= (predio-material pr) 2]
     COR-MATEIRAL2)
    ([= (predio-material pr) 3]
     COR-MATERIAL3)))
;; testes:


;; desenha-lista-predios: lista-de-predios -> Imagem
;; desenha uma lista de predios em uma dada cena
(define (desenha-lista-predios predios cena)
  (local
    ((define predios-validos (filter (λ (p) (not (= (predio-impactos p) 0))) predios)))
    (foldr coloca-predio cena predios-validos)))

(define-struct meteoro (pos-x pos-y tamanho direcao vel material forca))
;; uma estrutura do tipo meteoro tem o seguinte formato:
;; (make-meteoro x y t d v m f), onde:
;; x: Número, posicao x do meteoro
;; y: Número, posicao y do meteoro
;; t: Número, tamanho do raio do meteoro
;; d: Símbolo, representa se ele cai para a direita ou a esquerda
;; v : Número, representa a velocidade (incremento)
;; m: Número de 1 a 3, material do meteoro
;; f: Número, poder do impacto do meteoro
;; constantes:
(define METEORO1 (make-meteoro 20 200 10 'd 10 1 0))
(define METEORO2 (make-meteoro 50 0 15 'e 1 2 0))
(define METEORO3 (make-meteoro 350 0 5 'e 1 3 0))
;; uma lista-de-meteoros é:
;; 1. empty, ou
;; 2. (cons m lm), onde:
;; m: meteoro
;; lm: lista-de-meteoros
;; que, ainda, obedece ao predicado (listof meteoro?)

;; uma lista-imagens é:
;; 1. empty
;; 2. (cons i li), onde:
;; i = imagem
;; li = lista-imagens
;; e que, ainda, obecede ao predicado (listof image?)

;; desenha-meteoro: meteoro -> Imagem
;; desenha um meteoro, só isso mesmo
(define (desenha-meteoro met)
  (circle (meteoro-tamanho met) "solid" (escolhe-cor-meteoro met)))

;; escolhe-cor-meteoro: meteoro -> String
;; dado um meteoro, devolve a cor do material
(define (escolhe-cor-meteoro pr)
  (cond
    ([= (meteoro-material pr) 1]
     COR-METEORO1)
    ([= (meteoro-material pr) 2]
     COR-METEORO2)
    ([= (meteoro-material pr) 3]
     COR-METEORO3)))
   
;; desenha-meteoro-cainho: meteoro Imagem -> lista-imagens
;; desenha n cenas de um meteoro até ele cair no chão, ou sair de cena
(define (desenha-meteoro-caindo met c)
  (cond
    [(meteoro-caiu? met c) empty]
    [else (cons (place-image (desenha-meteoro met) (meteoro-pos-x met) (meteoro-pos-y met) c)
                (desenha-meteoro-caindo (avanca-meteoro met) c))]))
    
;; meteoro-caiu? : meteoro imagem -> booleano
;; testa se um meteoro caiu no chao ou saiu de cena
(define (meteoro-caiu? met c)
  (let
      ([max-x (+ (meteoro-pos-x met) (meteoro-tamanho met))]
       [max-y (+ (meteoro-pos-y met) (meteoro-tamanho met))])
    (cond
      [(>= max-x CENA-LARGURA)
       #t]
      [(>= max-y (+ CENA-ALTURA 50))
       #t]
      [(<= max-x 0) #t]
      [else #f])))
;; avanca-meteoro: meteoro -> meteoro
;; cria um novo meteoro com base no anterior, avançado conforme a velocidade, MRU
(define (avanca-meteoro met)
  (cond
    [(symbol=? (meteoro-direcao met) 'd)
     (make-meteoro
      (+ (meteoro-vel met)(meteoro-pos-x met))
      (+ (meteoro-vel met)(meteoro-pos-y met))
      (meteoro-tamanho met)
      (meteoro-direcao met)
      (meteoro-vel met)
      (meteoro-material met)
      (meteoro-forca met))]
    [else
     (make-meteoro
      (- (meteoro-pos-x met)(meteoro-vel met))
      (+ (meteoro-vel met)(meteoro-pos-y met))
      (meteoro-tamanho met)
      (meteoro-direcao met)
      (meteoro-vel met)
      (meteoro-material met)
      (meteoro-forca met))]))

;; desenha-contorno : predio -> imagem
;; desenha o contorno de um prédio com base na vida restante
(define (desenha-contorno pred)
  (let ([cor
         (cond
           [(= (predio-impactos pred) 1)
            "red"]
           [(= (predio-impactos pred) 2)
            "DarkOrange"]
           [(= (predio-impactos pred) 3)
            "Orange"]
           [(= (predio-impactos pred) 4)
            "Yellow"]
           [(= (predio-impactos pred) 5)
            "Green"])])
    (rectangle (predio-base pred) (predio-altura pred) "outline" cor)))

;; meteoro-impacto: meteoro predio -> booleano
;; testa e atualiza o predio com base no impacto ou não do meteoro
(define (meteoro-impacto met pred)
  (local ([define altura-predio (- POSICAO-Y-PREDIOS (/ (predio-altura pred) 2))]
        [define x-direita-predio (+ (predio-pos pred) (/ (predio-base pred) 2))]
        [define x-esquerda-predio (- (predio-pos pred) (/ (predio-base pred) 2))]
        [define altura-meteoro (+ (meteoro-pos-y met) (meteoro-tamanho met))]
        [define x-direita-meteoro (+ (meteoro-pos-x met) (meteoro-tamanho met))]
        [define x-esquerda-meteoro (- (meteoro-pos-x met) (meteoro-tamanho met))]
        [define colidiu? (and (>= altura-meteoro altura-predio)
                           (and (<= x-esquerda-meteoro x-direita-predio)
                                (>= x-direita-meteoro x-esquerda-predio)))])
    colidiu?))

(define (chuva-de-meteoros met lista-p cena)
  (local ((define meteoro-colisao? (ormap (λ (p) (meteoro-impacto met p)) lista-p))
          (define novos-predios (atualiza-predios met lista-p)))
    (cond
      [(meteoro-caiu? met cena)
       (list (desenha-lista-predios novos-predios cena))]
      [meteoro-colisao? (list (desenha-lista-predios novos-predios cena))]
      [else (cons (place-image (desenha-meteoro met) (meteoro-pos-x met) (meteoro-pos-y met) (desenha-lista-predios novos-predios cena))
                  (chuva-de-meteoros (avanca-meteoro met) novos-predios cena))])))

;; atualiza-predios: meteoro lista-de-predios -> lista-de-predios
;; atualiza os impactos de um prédio com base no meteoro
(define (atualiza-predios met lista-p)
  (local
    ((define (atualiza-vida predio)
       (if (>= (predio-impactos predio) 0) 0 (- (predio-impactos predio) (meteoro-forca met))))
     (define (predio-atingido p)
       (make-predio
        (predio-altura p)
        (predio-base p)
        (predio-pos p)
        (predio-material p)
        (atualiza-vida p))))
    (map (λ (p) (if (meteoro-impacto met p) (predio-atingido p) p)) lista-p)))


;; inicializa-meteoro: meteoro -> meteoro
;; inicializa a força de um meteoro com base em seu material
(define (inicializa-meteoro m)
  (local ((define forca-meteoro
            (cond
              [(= (meteoro-material m) 1) 1]
              [(= (meteoro-material m) 2) 3]
              [(= (meteoro-material m) 3) 5]
              [else (error "meteoro com material inválido")])))
     (make-meteoro
      (meteoro-pos-x m)
      (meteoro-pos-y m)
      (meteoro-tamanho m)
      (meteoro-direcao m)
      (meteoro-vel m)
      (meteoro-material m)
      forca-meteoro)))


;; grande-chuva-de-meteoros: lista-de-meteoros lista-de-predios imagem -> lista-imagens
(define (grande-chuva-de-meteoros meteoros predios cena)
  (local
    ((define terminou? (andmap (λ (m) (meteoro-acabou? m predios cena)) meteoros))
     (define novos-meteoros (filter (λ (m) (not (meteoro-acabou? m predios cena))) meteoros)))
    (if terminou?
        (list cena)
        (local
          ((define novos-predios (atualiza-predios-lista meteoros predios))
           (define cena-com-predios (desenha-lista-predios novos-predios cena))
           (define coloca-meteoros (foldr (λ (m c) (place-image (desenha-meteoro m) (meteoro-pos-x m) (meteoro-pos-y m) c)) cena-com-predios novos-meteoros))
           (define avanca-lista-meteoros (map avanca-meteoro novos-meteoros)))
           (cons coloca-meteoros
                 (grande-chuva-de-meteoros avanca-lista-meteoros novos-predios cena))))))
(define lista-meteoros-inicializada
  (list
   (inicializa-meteoro METEORO1)
   (inicializa-meteoro METEORO2)
   (inicializa-meteoro METEORO3)))

;; atualiza-predios-lista: lista-de-meteoros lista-de-predios -> lista-de-predios
;; atualiza as informações de cada prédio para uma lista de meteoros
(define (atualiza-predios-lista lm lp)
  (cond
    [(empty? lm) lp]
    [else
     (atualiza-predios-lista
      (rest lm)
      (atualiza-predios (first lm) lp))]
    )
  )

;; meteoro-acabou?: meteoro lista-de-predios imagem -> booleano
;; dado um meteoro, testa se atingiu algum prédio ou atingiu a borda da cena
(define (meteoro-acabou? m lista-p cena)
  (or
   (ormap (λ (p) (meteoro-impacto m p)) lista-p)
   (meteoro-caiu? m cena)))


(run-movie 0.08 (grande-chuva-de-meteoros lista-meteoros-inicializada LISTAP1I CENA))