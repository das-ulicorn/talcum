;;; $Id: talcum-pikeman.el,v 1.24 2005/09/13 18:30:01 ulmi Exp $
;;;
;;; This file is part of Talcum.
;;;
;;; Talcum is released under the terms and conditions
;;; of the LaTeX Project Public License 1.3a (see file
;;; LPPL-13a for details)
;;;
;;; Talcum has the maintenance status: author-maintained.
;;; Maintainer: Ulrich M. Schwarz, ulmi@users.sarovar.org.
;;; 

(require 'talcum-tools)
(require 'talcum-compat)

(talcum-namespace
 (~~sourcefile-versionstamp "$RCSfile: talcum-pikeman.el,v $"  
                            "$Revision: 1.24 $"))

(defgroup talcum-pikeman nil "The compiling options menu" :group 'talcum)

(talcum-namespace

 (defvar talcum-pikeman-compile-options-functions nil
   "A list of functions that will be called prior to calling tex-file.
Each function will be called with one argument, the name of the main
file, and should return a string. All these strings are concatenated,
seperated by spaces, and passed as parameters to TeX.")

 (defvar talcum-pikeman-compile-filestart-functions nil
   "A list of functions that will be called prior to calling tex-file.
Each function will be called with one argument, the name of the main
file, and should return a string. All these strings are concatenated,
and passed to TeX as the first input line, suitably quoted.")

 (defvar talcum-pikeman-menu-creator-functions nil
   "A list of functions that are called with no parameters to create
the Pikeman menu. They should return a list of menu items, since all
the results are appended.")

 (defun talcum-pikeman-generate-menu ()
   (list "--"
         (~~pikeman-generate-menu)))

 (add-to-list 'talcum-menu-functions #'talcum-pikeman-generate-menu)

 (defun ~~pikeman-generate-menu ()
   (list "Pikeman"
         :filter #'(lambda (_)
                   (apply 'append
                          (mapcar
                           (lambda (mcf) (funcall mcf))
                           talcum-pikeman-menu-creator-functions)))))

 (defun ~~pikeman-compiler-options (texfile)
   "Assemble the command-line options for the tex call (by calling
talcum-pikeman-compile-options-functions). TEXFILE is the name of the
main file, with extension and possibly path."
   (let ( (options       ; we do a little dance to exclude nil entries
          (mapconcat #'identity        ; just to splice in the spaces
                     (~~filter #'identity ; to remove the nils
                               (mapcar (lambda (optfun) (funcall optfun texfile)) 
                                       talcum-pikeman-compile-options-functions))
                     " ")))
     options))

 (defun ~~pikeman-first-line (texfile)
   "Assemble the first line for the tex call (by calling
talcum-pikeman-compile-filestart-functions). TEXFILE is the name of
the main file, with extension and possibly path."
   (let ((firstline (mapconcat (lambda (filifun) (funcall filifun texfile)) 
                               talcum-pikeman-compile-filestart-functions "")))
     firstline))

 (defun ~~pikeman-prepare-compiler-string ()
   "Set tex-command to include compiler options and the first line."
   (let* ((texfile (tex-main-file))
          (options (~~pikeman-compiler-options texfile))
          (firstline (~~pikeman-first-line texfile)))
     (setq tex-command
           (concat latex-run-command " "
                   ;;"--jobname * " ;; * not quoted, replaced by tex-file
                   (~~value-safe tex-start-options-string) ;; removed in 21--
                   (~~value-safe tex-start-options) ;; 22+
                   " " 
                   options " "
                   " '" 
                   ;; somewhat ugly bugfix: what do we do about *
                   ;; generated by tex-start-commands, tpc-filestart
                   ;; et al.? If allowed, the \input below will fail,
                   ;; since at most one * expansion is done.
                   ;; * in options can still give replacement.
                   ;; We assume existence of \def\@gobble#1{} (i.e. LaTeX 2e)
                   "\\csname @gobble\\endcsname{*}" 
                   firstline 
                   (~~value-safe tex-start-commands) ;; 22+
                   "\\input{"
                   texfile
                   "}'"))
     ))

 ;; Only needed for classic LaTeX mode, but who cares?
 (defadvice tex-file (before talcum-pikeman activate)
   (~~pikeman-prepare-compiler-string))

 ;; Only for AucTeX. Can't check for AucTeX version since that is
 ;; sometimes a date, sometimes not. We only provide the %()-vars,
 ;; it's the user's job to figure out where these go.
 ;; Possibly, changing the "LaTeX" entry of TeX-command-list to
 ;; "%l %(Talcum-options) '%(mode)%(Talcum-line1)\\input{%t}'"
 ;; will work. Windows users might need to change the ' to \".
 ;;
 ;; Talcum's interaction selector will probably override AucTeX', so
 ;; will the format selector. Since AucTeX' mechanism is explicitly
 ;; flagged as to be redone, no work will be put into improving
 ;; interaction.
 (when (boundp 'TeX-expand-list)
   (add-to-list 'TeX-expand-list
        (list "%(Talcum-options)" 
              (lambda () (~~pikeman-compiler-options (TeX-active-master t)))))
   (add-to-list 'TeX-expand-list
        (list "%(Talcum-line1)" 
              (lambda () (~~pikeman-first-line (TeX-active-master t))))))

); namespace
;;; That should be sufficient for the framework.



;;; Now, the canonical examples (to be out-sourced � la TREX)

;;; Ex. 1: Includes.

(talcum-namespace

 (defcustom talcum-pikeman-chapters-preselect-function
   'talcum-pikeman-chapters-preselect-current
   "This variable's value should be a function. It will be called with
the list of chapters when Talcum is activated and should return the
initially-selected chapters."
   :group 'talcum-pikeman
   :type '(choice
           (const :tag "No chapters" 
                  talcum-pikeman-chapters-preselect-none)
           (const :tag "All chapters"
                  talcum-pikeman-chapters-preselect-all)
           (const :tag "Chapter of buffer"
                  talcum-pikeman-chapters-preselect-current)
           (const :tag "Keep choice"
                  talcum-pikeman-chapters-preselect-keep)
           function
           ))
 
 ;; to be made buffer-local? we want one per main file...
 (defvar ~~pikeman-chapters-selected-chapters nil)
 (make-variable-buffer-local (quote ~~pikeman-chapters-selected-chapters))
 (defvar ~~pikeman-chapters-all-chapters nil)
 (make-variable-buffer-local (quote ~~pikeman-chapters-all-chapters))

 ;; Four useful choices for talcum-pikeman-chapters-preselect-function:

 ;; Don't select anything, like earlier versions did.
 (defun talcum-pikeman-chapters-preselect-none (chaps)
   nil)

 ;; Select all chapters.
 (defun talcum-pikeman-chapters-preselect-all (chaps)
   chaps)

 ;; Select the current file only.
 (defun talcum-pikeman-chapters-preselect-current (chaps)
   (message "selecting current")
   (let ((this-file (file-name-nondirectory 
                     (file-name-sans-extension (buffer-file-name)))))
     (if (member this-file chaps)
         (list this-file)
       chaps)))

 ;; Intersect current choice with currently feasible.
 (defun talcum-pikeman-chapters-preselect-keep (chaps)
   (~~filter #'(lambda (chap) (member chap ~~pikeman-chapters-selected-chapters)) chaps))


 (defun talcum-pikeman-chapters-preselect ()
   ;; Since the value is now buffer-local, we set the default as well.
   ;; In particular, talcum-pikeman-chapters-preselect-function will
   ;; still see the default value (= value from last opened file) and
   ;; may choose to keep it.
   (set-default (quote ~~pikeman-chapters-selected-chapters)
                (setq ~~pikeman-chapters-selected-chapters
                      (funcall talcum-pikeman-chapters-preselect-function 
                               (~~pikeman-chapters-find-chapters))))
   (~~debug 4 "Selected chapters: %s" ~~pikeman-chapters-selected-chapters)
   t)

 ;; Drek! At that point, we don't usually have the main file pinned down.
 ; We cannot hack-local-variable unless we 
 ; (add-hook 'talcum-enter-mode-hook #'talcum-pikeman-chapters-preselect)
 
 ;; "Ugly as sin" doesn't begin to cover this, but it seems to work,
 ; sort of. We are out of luck if something compiles without user
 ; interaction. (Any command executed, and we're set up. Our menu
 ; opened, and we're set up.)

 (defun ~~pikeman-chapters-preselect-and-selfdestruct ()
     (talcum-pikeman-chapters-preselect)
     (remove-hook 'pre-command-hook (quote ~~pikeman-chapters-preselect-and-selfdestruct))
     (setq talcum-pikeman-menu-creator-functions
           (delete (quote ~~pikeman-chapters-preselect-and-selfdestruct)
                   talcum-pikeman-menu-creator-functions))
   ;; no, no menu, really. We're here for the side effects.
   '())

 (add-hook 'talcum-enter-mode-hook
           #'(lambda () 
               ;; On any command. Unfortunately, opening a menu isn't a command ...
               ;; so the menu code tries as well.
               (add-hook 'pre-command-hook 
                         (quote ~~pikeman-chapters-preselect-and-selfdestruct))
               ;; Dynamic menus to the rescue! This must be after the
               ;; entry for -include-menu, because items are consed
               ;; on. Luckily, -include-menu is added at require-time,
               ;; and that is before enter-mode-hook is run. Phew.
               (add-to-list 'talcum-pikeman-menu-creator-functions
                            (quote ~~pikeman-chapters-preselect-and-selfdestruct))
               ))
 
 (defun ~~pikeman-chapters-find-chapters ()
   (~~pikeman-chapters-find-includes 
    (or (and (boundp 'tex-main-file) 
             tex-main-file)
        (and (talcum-auctex-p) 
             (TeX-active-master t))
        nil)))

 (defun ~~pikeman-chapters-include-menu ()

   (setq ~~pikeman-chapters-all-chapters (~~pikeman-chapters-find-chapters))
   (when ~~pikeman-chapters-all-chapters 
       ;; if nothing to select, don't offer menu.
       ;; Caveat: we look at version on disk.
     (identity ;; was list for submenu, but that's a lot of mousing.
      (append '("--" "--"
                ["Included chapters:" nil]
                )
              (mapcar (quote ~~pikeman-chapters-menu-item) 
                      ~~pikeman-chapters-all-chapters)
              '("---"
                ["Include none" 
                 (talcum-namespace
                  (setq ~~pikeman-chapters-selected-chapters nil))]
                
                ["Include all"
                 (talcum-namespace
                  (setq ~~pikeman-chapters-selected-chapters 
                        ~~pikeman-chapters-all-chapters))]

                ["Rescan file"
                 (talcum-namespace
                  (setq ~~pikeman-chapters-all-chapters (~~pikeman-chapters-find-includes)))])
              ))))

 (defun ~~pikeman-chapters-menu-item (chapter)
   `[,chapter 
     (talcum-namespace
      (setq ~~pikeman-chapters-selected-chapters  
            (if (member ,chapter ~~pikeman-chapters-selected-chapters)
                (remove ,chapter ~~pikeman-chapters-selected-chapters)
              (cons ,chapter ~~pikeman-chapters-selected-chapters))))
     :key-sequence nil                  ;sorry, how would that work?
     :style toggle
     :selected (member ,chapter 
                       (quote ,(talcum-namespace ~~pikeman-chapters-selected-chapters)))
     ])
   
 (defun ~~pikeman-chapters-find-includes (&optional buffer-or-file)
   (interactive "bMain buffer")
   (let ((buffer (or (and (bufferp buffer-or-file) buffer-or-file)
                     (and (stringp buffer-or-file) 
                          (let* ((bu (generate-new-buffer "Talcum temp master")))
                            (save-excursion
                              (set-buffer bu)
                              (insert-file-contents buffer-or-file nil nil nil t)
                              bu)))
                     (current-buffer)))
         (rst nil)
         (inhibit-point-motion-hooks t)
         (inhibit-redisplay t))
     (save-excursion
       (set-buffer buffer)
       (goto-char (point-min))
       (while (search-forward-regexp 
               "^[[:space:]]*\\\\include{\\([^}]+\\)}" 
               (point-max) t)
         (push (match-string-no-properties 1) rst))
       (setq rst (nreverse rst))
       ;;(message "%s" rst)
       (if (stringp buffer-or-file)
           (kill-buffer buffer))
       rst)))

 (defun ~~pikeman-chapters-make-includeonly (file)
   ;; if we use all chapters, give no includeonly at all.
   (unless (= (length ~~pikeman-chapters-all-chapters)
              (length ~~pikeman-chapters-selected-chapters))
     (concat "\\includeonly{"
             (mapconcat 'identity 
                        ~~pikeman-chapters-selected-chapters 
                        ",")
             "}")))


 (add-to-list 'talcum-pikeman-compile-filestart-functions
              (quote ~~pikeman-chapters-make-includeonly))
 (add-to-list 'talcum-pikeman-menu-creator-functions
              (quote ~~pikeman-chapters-include-menu))

 )                                      ; namespace

;; Ex. 2: setting the interaction mode
(talcum-namespace

 (defvar ~~pikeman-interaction-mode nil)

 (defun ~~pikeman-interaction-menu ()
   (list (cons "Interaction mode"
         (~~menu-choose-one ~~pikeman-interaction-mode
                            '(("(default)" nil)
                             ("batch" "batchmode")
                             ("non stop" "nonstopmode")
                             ("scroll" "scrollmode")
                             ("errorstop" "errorstopmode"))))))

 (defun ~~pikeman-interaction-option (file)
   (if ~~pikeman-interaction-mode
       (concat "--interaction " ~~pikeman-interaction-mode)))

 (add-to-list 'talcum-pikeman-compile-options-functions
              (quote ~~pikeman-interaction-option))
 (add-to-list 'talcum-pikeman-menu-creator-functions
              (quote ~~pikeman-interaction-menu))
 ); namespace

;; Ex. 3: setting the format
(talcum-namespace

 (defcustom talcum-pikeman-formats
   '(("(default)" nil)
     ("pdfLaTeX" "pdflatex")
     ("LaTeX" "latex")
     ;;; better not confuddle the users.
     ;;("Beamer" "beamer")
     ;;("Diplom" "diplom")
     )
   "Formats to offer in the Output Format menu. First element is the
menu name, second is the fmt file name."
   :group 'talcum-pikeman
   :type '(repeat 
           (list (string :tag "Menu name") 
                 (choice :tag "Format"
                         (string :tag "File name")
                         (const :tag "(none)" nil)))))

 (defvar ~~pikeman-format nil)

 (defun ~~pikeman-format-menu ()
   (list (cons "Output format"
   ;(cons "---"
         (~~menu-choose-one ~~pikeman-format
                            talcum-pikeman-formats))))

(defun ~~pikeman-format-option (file)
  (if ~~pikeman-format
      (concat "&" ~~pikeman-format)))

 (add-to-list 'talcum-pikeman-compile-options-functions
              (quote ~~pikeman-format-option))
 (add-to-list 'talcum-pikeman-menu-creator-functions
              (quote ~~pikeman-format-menu))
);namespace

;; Ex. 4: \listfiles.
(talcum-namespace
 (defvar ~~pikeman-listfiles nil)
 (defun ~~pikeman-listfiles-menu ()
   (list ;"--"
         '["\\listfiles" 
           (talcum-namespace (~~negate ~~pikeman-listfiles))
           :style toggle
           :selected (talcum-namespace ~~pikeman-listfiles)]))
 (defun ~~pikeman-make-listfiles (file)
   (if ~~pikeman-listfiles "\\listfiles"))

 (add-to-list 'talcum-pikeman-menu-creator-functions
              (quote ~~pikeman-listfiles-menu))
 (add-to-list 'talcum-pikeman-compile-filestart-functions
              (quote ~~pikeman-make-listfiles))
); namespace

;; Ex. 5: draft or final
(talcum-namespace
 (defvar ~~pikeman-draftmode nil)
 (defun ~~pikeman-draftmode-menu ()
   (list 
    '["Draft" 
      (talcum-namespace (~~negate ~~pikeman-draftmode))
      :style toggle
      :selected (talcum-namespace ~~pikeman-draftmode)]))
 ;; Effectively, the user's documentclass option takes precedence,
 ;; since it is evaluated after our setting. This means we cannot
 ;; override explicit "draft" anyway, so a "final" setting is moot.
 ;;     (cons "Draft/Final"
 ;;                (~~menu-choose-one ~~pikeman-draftmode
 ;;                                   '(("(default)" nil)
 ;;                                    ("draft" "draft")
 ;;                                    ("final" "final"))))))
 (defun ~~pikeman-make-draftmode (file)
   (when ~~pikeman-draftmode
     (save-excursion
       (let ((bu (generate-new-buffer "Talcum temp master")))
         (set-buffer bu)
         (insert-file-contents file nil nil nil t)
         (goto-char (point-min))
         (if (search-forward-regexp 
              "\\\\documentclass\\(\\[[^]]*\\]\\)?{\\([^}]+\\)}" 
              nil t)
             ;; XXX: this fails for {...]...} within class options.
             ;; Does anybody do that?
             (when ~~pikeman-draftmode
               (format ;; "\\PassOptionsToClass{%s}{%s}" 
                ;; Grrr... POTC declares options _local to the class_.
                ;; This means the packages don't get to see the draft or
                ;; final. We do a very dirty hack instead, because there
                ;; is no interface to declare global class options.
                ;; This is the point where Joe Normal should look away.
                (concat
                 "\\makeatletter"
                 "\\xdef\\@classoptionslist{"
                 "draft"
                 "\\ifx\\@classoptionslist\\relax\\else,\\@classoptionslist\\fi}"
                 "\\makeatother")
                (or (match-string 2)  ;; in case of options
                    (match-string 1)) ;; in case of no options
                )))))))

 (add-to-list 'talcum-pikeman-menu-creator-functions
              (quote ~~pikeman-draftmode-menu))
 (add-to-list 'talcum-pikeman-compile-filestart-functions
              (quote ~~pikeman-make-draftmode))
);namespace

;; Ex. 6: force non-embedding of base fonts into pdf.
;;
;; This is very much not recommended. However, I believe that there
;; are rare cases where the file size justifies the severe loss of
;; quality.
(talcum-namespace
 (defvar ~~pikeman-nonembed-fonts nil)
 (defun ~~pikeman-nonembed-menu ()
   (list ;"--"
         '["Don't embed base 14 fonts (PDF only)" 
           (talcum-namespace (~~negate ~~pikeman-nonembed-fonts))
           :style toggle
           :selected (talcum-namespace ~~pikeman-nonembed-fonts)]))
 (defun ~~pikeman-make-nonembed (file)
   ;; Yeah, I know, this is not going to work for anybody else.
   (if ~~pikeman-nonembed-fonts "\\pdfmapfile{pdftex_ndl14}"))

 (add-to-list 'talcum-pikeman-menu-creator-functions
              (quote ~~pikeman-nonembed-menu))
 (add-to-list 'talcum-pikeman-compile-filestart-functions
              (quote ~~pikeman-make-nonembed))
 );namespace

;; Ex. 7: source specials.
(talcum-namespace
 (defvar ~~pikeman-srcspc nil)
 (defun ~~pikeman-srcspc-menu ()
   (list ;"--"
         '["Source specials" 
           (talcum-namespace (~~negate ~~pikeman-srcspc))
           :style toggle
           :selected (talcum-namespace ~~pikeman-srcspc)]))
(defun ~~pikeman-srcspc-option (file)
  (if ~~pikeman-srcspc
      "-src-specials"))

 (add-to-list 'talcum-pikeman-compile-options-functions
              (quote ~~pikeman-srcspc-option))
 (add-to-list 'talcum-pikeman-menu-creator-functions
              (quote ~~pikeman-srcspc-menu))

); namespace

(provide 'talcum-pikeman)

;;; talcum-pikeman.el ends here.