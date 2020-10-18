
;; i just need something to run cube algorthms
;; in a recursive way; wow


(define (typein simb)
  (string->symbol
   (string-append (symbol->string '<)
		  (string-upcase (symbol->string simb)) 
		  (symbol->string '>))))
		   
(define ColorsCube
	 '((<color> RED)
	   (<color> GREEN)
	   (<color> BLUE)
	   (<color> YELLOW)
	   (<color> ORANGE)
	   (<color> WHITE)
	   (<color> MAGENTA)
	   (<color> SIANO)))

(define (makeLine n simbol)
  "create a line with n nows and a simbol"
  (define (makeLineAps simbolof n line)
    (if (= n 0)
	(cons (typein 'line) line)
	(makeLineAps simbolof
		     (- n 1)
		     (cons simbol line))))
  (makeLineAps simbol n '()))


(define (makeFace n simbol)
  "create a face with a pre-determinated simbol"
  (define (makeFaceAps i simbol face)
    (if (= i 0)
	(cons (typein 'face) face)
	(makeFaceAps (- i 1)
		     simbol
		     (cons (makeLine n simbol) face))))
  (makeFaceAps n simbol '()))


(define (makeCube n colorsCube)
  "Create a cube with always 6 faces and any number of rows"
  (define (makeCubeAps n colors sides cube)
    (if (= n 0)
	(cons (typein 'cube) cube)
	(makeCubeAps (- n 1)
		     (cdr colors)
		     sides
		     (cons (makeFace sides (car colors)) cube))))
  (makeCubeAps 6 colorsCube n '()))


(define cube (makeCube 3 ColorsCube))




