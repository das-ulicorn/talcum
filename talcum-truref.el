;;;!; $Id: talcum-truref.el,v 1.2 2007/01/30 20:49:39 ulmi Exp $
;;;!;
;;;!; This file is part of Talcum.
;;;!;
;;;!; Talcum is released under the terms and conditions
;;;!; of the LaTeX Project Public License 1.3a (see file
;;;!; LPPL-13a for details)
;;;!;
;;;!; Talcum has the maintenance status: author-maintained.
;;;!; Maintainer: Ulrich M. Schwarz, ulmi@users.sarovar.org.
;;;!; 

;;; Referencing equations by their displayed number.
;;;
;;; Immensely useful if you have a compiled version open for
;;; proofreading. We use information from the aux files. As a
;;; consequence, we can only use data from past runs. If your auxfile
;;; is stale, you're officially fscked.
;;;
;;; Keybinding is C-c # ) to mimic RefTeX' C-c ).
;;;
;;; The label string has to pass a test defined by
;;; ~~truref-label-looks-like-equation. Currently, this means having
;;; the form eq:mumble. We need some test because sections etc. also
;;; only have numbers. Furthermore, the number needs to be unique
;;; (among equations), since we parse the aux files breadth-first and
;;; not depth-first.
;;;
;;; TODO:
;;; - test more.
;;; - handle that the user might enter the parens around the number.
;;; - prompt with completion, hack completion window to show context a la RefTeX?
;;; - when hyperref is used, the 2.4th argument to newlabel can be used to spot equations.
;;; - customization.

(require 'talcum-tools)

;; The format of aux file entries is:
;; - without hyperref:
;; \newlabel{eq:1}{{3}{2}}, \newlabel{eq:4}{{2b}{1}}
;; i.e. {label}{{number}{page}}
;;
;; - with hyperref:
;; \newlabel{eq:2}{{1}{1}{\relax }{equation.1}{}}
;; \newlabel{eq:3}{{2a}{1}{\relax }{equation.2a}{}}
;; i.e. {label}{{number}{page}{something}{anchor}}
;;
;; subfiles are included using \@input{test2.aux}

(talcum-namespace
 (~~sourcefile-versionstamp "$RCSfile: talcum-truref.el,v $" "$Revision: 1.2 $")

 (defconst talcum-truref-refcall "\\eqref{%s}")

 (defun talcum-truref-simple (eqno)
   (interactive "sEquation number: ")
   (let* ((mainfile (file-name-sans-extension
                     (cond
                      ((and ;(equal talcum-definitions-file ':latexmode)
                        (boundp 'tex-main-file)
                        (stringp tex-main-file))
                       tex-main-file)
                      ((talcum-auctex-p)
                       (TeX-active-master t))
                      (t (buffer-file-name))
                      )))
          (inhibit-point-motion-hooks t)
          (inhibit-redisplay t)
          )
     (insert (format talcum-truref-refcall 
                     (or
                      (~~truref-lookup-eqno eqno (format "%s.aux" mainfile))
                      (error "No equation no. %s found" eqno))))))

 ;; Find a label with text eqno, starting in thefile, but tracing
 ;; @input commands, if necessary. thefile has extension aux.
 ;; Return the label, or nil if not found.
 ;;
 ;; We may assume that eqno is vaguely unique in the document, so we
 ;; may check all labels in thefile before recursing into subfiles, if
 ;; that is convenient.
 (defun ~~truref-lookup-eqno (eqno thefile)
   (let* ((result nil))
     (with-temp-buffer
       (insert-file-contents thefile)
       ;; trying local file first:
       (goto-char (point-min))
       (while (and (not result)
                   (search-forward-regexp "\\\\newlabel{\\([^}]+\\)}{{\\([^}]+\\)}[^}]*}" nil t))
         (when (and (equal eqno (match-string 2))
                    (~~truref-label-looks-like-equation (match-string 1)))
           (setq result (match-string 1))))
       ;; trying included files now:
       (goto-char (point-min))
       (while (and (not result)
                   (search-forward-regexp "\\\\@input{\\(.*\\.aux\\)}" nil t))
         (setq result (~~truref-lookup-eqno eqno (match-string 1)))))
       result))

   (defun ~~truref-label-looks-like-equation (str)
     (save-match-data
       (string-match "eq:.*" str)))
); namespace

;;;
;;; Key bindings
(define-key talcum-keymap ")" 'talcum-truref-simple) ; reftex' is C-c )

(provide 'talcum-truref)
;;;!; talcum-truref.el ends here.
