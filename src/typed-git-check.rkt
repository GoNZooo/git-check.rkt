#lang typed/racket/base

(require racket/file
         racket/port
         racket/system
         racket/match
         racket/cmdline

         ; Threading macro (`~>`) by Jay McCarthy (jeapostrophe)
         ; https://github.com/jeapostrophe/exp/blob/master/threading-arrow.rkt
         jeapostrophe/threading-arrow)

(: find-git-directories (->* () (Path-String) (Listof Path)))
(define (find-git-directories [path "."])
  (: has-git-dir? (-> Path-String Boolean))
  (define (has-git-dir? dir)
    (directory-exists? (build-path dir ".git")))

  (find-files has-git-dir?
              path))

(: git-changed? (-> Path Boolean))
(define (git-changed? path)
  (begin
    (define old-path (current-directory))
    (current-directory path)
    (define git-status-output (with-output-to-string
                                (lambda ()
                                  (system "git status -s"))))
    (current-directory old-path)
    (not (equal? git-status-output
                 ""))))

(: top-directories (->* ((Listof Path)) ((U Path False) (Listof Path))
                        (U Null (Listof Path))))
(define (top-directories directories [last #f] [output '()])
  (cond
    [(null? directories) output]
    [(not last)
     (top-directories (cdr directories)
                      (car directories))]
    [else
      (match directories
        [(list next others ...)
         #:when (regexp-match? (path->string last)
                               (path->string next))
         (top-directories others
                          last
                          output)]
        [(list next others ...)
         (top-directories others
                          next
                          (cons last
                                output))])]))

(: announce (-> Path Void))
(define (announce path)
  (printf "~a should be checked.~n"
          path))

(: root-dir (Parameterof Path))
(define root-dir (make-parameter (current-directory)))

(: command-line-options (-> Void))
(define (command-line-options)
  (command-line
    #:once-each
    [("-d" "--dir")
     #{d : Path-String}
     "Specify a root directory to search"
     (root-dir (expand-user-path d))]))

(module+ main
  (command-line-options)
  (for-each announce
            (~> (root-dir)
                find-git-directories
                top-directories
                (filter git-changed? <>))))
