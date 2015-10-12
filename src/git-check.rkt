#lang racket/base

(require racket/file
         racket/port
         racket/system
         racket/match
         
         ; Threading macro (`~>`) by Jay McCarthy (jeapostrophe)
         ; https://github.com/jeapostrophe/exp/blob/master/threading-arrow.rkt
         jeapostrophe/threading-arrow)

(define (find-git-directories [path "."])
  (find-files (lambda (f)
                (directory-exists? (build-path f ".git")))
              path))

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

(define (top-directories directories [last ""] [output '()])
  (cond
    [(null? directories) output]
    [(equal? last
              "")
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

(module+ main
  (require racket/pretty)

  (pretty-print
    (~> (current-directory)
        find-git-directories
        top-directories
        (filter git-changed? <>))))
