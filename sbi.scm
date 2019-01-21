#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/racket/bin/mzscheme -qr
;; $Id: sbi.scm,v 1.10 2019-01-15 14:10:54-08 - - $
;;
;; NAME
;;    sbi.scm - silly basic interpreter
;;
;; SYNOPSIS
;;    sbi.scm filename.sbir
;;
;; DESCRIPTION
;;    The file mentioned in argv[1] is read and assumed to be an SBIR
;;    program, which is the executed.  Currently it is only printed.
;;

(define *stdin* (current-input-port))
(define *stdout* (current-output-port))
(define *stderr* (current-error-port))

(define *run-file*
    (let-values
        (((dirpath basepath root?)
            (split-path (find-system-path 'run-file))))
        (path->string basepath))
)

(define (die list)
    (for-each (lambda (item) (display item *stderr*)) list)
    (newline *stderr*)
    (exit 1)
)

(define (usage-exit)
    (die `("Usage: " ,*run-file* " filename"))
)

(define (readlist-from-inputfile filename)
    (let ((inputfile (open-input-file filename)))
         (if (not (input-port? inputfile))
             (die `(,*run-file* ": " ,filename ": open failed"))
             (let ((program (read inputfile)))
                  (close-input-port inputfile)
                         program))))

(define (dump-stdin)
    (let ((token (read)))
         (printf "token=~a~n" token)
         (when (not (eq? token eof)) (dump-stdin))))


(define (write-program-by-line filename program)
    (printf "==================================================~n")
    (printf "~a: ~s~n" *run-file* filename)
    (printf "==================================================~n")
    (printf "(~n")
    (for-each (lambda (line) (printf "~s~n" line)) program)
    (printf ")~n"))

(define (main arglist)
    (if (or (null? arglist) (not (null? (cdr arglist))))
        (usage-exit)
        (let* ((sbprogfile (car arglist))
               (program (readlist-from-inputfile sbprogfile)))
              (write-program-by-line sbprogfile program) (label-pass program) (interpret-program program))))
		

(define *symbol-table* (make-hash))
;(define (symbol-get key)
;        (hash-ref *symbol-table* key))
;(define (symbol-put! key value)
;        (hash-set! *symbol-table* key value))
		
(define *label-table* (make-hash))
;(define (label-get key)
;        (hash-ref *label-table* key))
;(define (label-put! key value)
;        (hash-set! *label-table* key value))
(define *function-table* (make-hash))
(define *variable-table* (make-hash))
		
(define (label-pass program)
	(let ((line (car program)))
		(if(and (not(null? (cdr line))) (symbol? (cadr line)))   
			(hash-set! *label-table* (cadr line) line )
			; (printf "~a: ~n" (car line))
			'()
		)
	)
	(if (not (null? (cdr program)))
		(label-pass (cdr program))
		(values)
	)
)

(define (interpret-program program)

	(define (interpret-next-line)
		(if (not (null? (cdr program)))
			(interpret-program (cdr program))
			(values)
		)
	)
	
	(define (interpret-statement statement)
		(let ((statement-return 			
			

							(cond 
							[(equal? (car statement) 'goto) (interpret-goto (cadr statement))]
							[(equal? (car statement) 'dim) "dim~n"]
							[(equal? (car statement) 'input) "input~n"]
							[(equal? (car statement) 'let) "let~n"]
							[(equal? (car statement) 'if) "if~n"]
							[(equal? (car statement) 'print) (interpret-print (cdr statement))]
							[else "not a recognized symbol"]
							)
		))
			(if (null? statement-return)
				(interpret-next-line)
				(interpret-program statement-return)
			)
		)
	)
	(let ((line (car program)))
		(cond 
			[(and (not(null? (cdr line))) (pair? (cadr line))) (interpret-statement (cadr line))]
			[(and (not(null? (cdr line))) (not(null? (cddr line))) (pair? (caddr line))) (interpret-statement (caddr line))]
			[else (interpret-next-line)]
		)
	)
)


(define (interpret-print arg)
	(cond 
		[(null?   arg) '()]
		[(string? (car arg)) (printf "~a" (car arg))]
		[else (printf "~a" (evalexpr (car arg)))]
	)
	(if(or (null? arg) (null? (cdr arg)))
		(begin (printf" ~n") '())		
		(interpret-print (cdr arg))
	)
)

(define (interpret-goto arg)
	(hash-ref *label-table* arg)
)

(define (evalexpr expr)
    (cond ((number? expr) (+ 0.0 expr))
          (else (let ((fn (hash-ref *function-table* (car expr)))
                      (args (map evalexpr (cdr expr))))
                     (apply fn args)))))





			  

(for-each
    (lambda (pair)
            (hash-set! *function-table* (car pair) (cadr pair)))
    `(

        
        (div     ,(lambda (x y) (floor (/ x y))))
        (log10   ,(lambda (x) (/ (log x) (log 10.0))))
        (mod     ,(lambda (x y) (- x (* (div x y) y))))
        (quot    ,(lambda (x y) (truncate (/ x y))))
        (rem     ,(lambda (x y) (- x (* (quot x y) y))))
        (+       ,+)
        (^       ,expt)
        (ceil    ,ceiling)
        (exp     ,exp)
        (floor   ,floor)
        (log     ,log)
        (sqrt    ,sqrt)
		(- ,-)
		(* ,*)
		(/ ,/)
		(abs    ,abs)
		(acos   ,acos)
		(asin   ,asin)
		(atan   ,atan)
		(cos    ,cos)
		(log    ,log)
		(round  ,round)
		(sin    ,sin)
		(sqrt   ,sqrt)
		(tan    ,tan)
		;(trunc  ,trunc)
		(<>     ,(lambda (x y) (not (eqv? x y))))
		(=      ,eqv?)
		(>      ,>)
		(<      ,<)
		(>=		,>=)
		(<=		,<=)

     ))
	 
;(for-each
    ;(lambda (pair)
         ;   (hash-set! *variable-table* (car pair) (cadr pair)))
	;(
		;(log10_2 0.301029995663981195213738894724493026768189881)
        ;(sqrt_2  1.414213562373095048801688724209698078569671875)
        ;(e       2.718281828459045235360287471352662497757247093)
        ;(pi      3.141592653589793238462643383279502884197169399)
	;	(eof     0.0)
		;(nan     ,(/ 0.0 0.0))
		
;	))
		
(printf "terminal-port? *stdin* = ~s~n" (terminal-port? *stdin*))
(if (terminal-port? *stdin*)
    (main (vector->list (current-command-line-arguments)))
    (printf "sbi.scm: interactive mode~n"))
	
