;;; esql.el ---                                      -*- lexical-binding: t; -*-

;; Copyright (C) 2019  

;; Author:  <antoine@antoine-AB350-Gaming>
;; Keywords: 

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:


;; TODO: checkout https://www.emacswiki.org/emacs/SqlMode#toc5
;; TODO: (nth 8 (syntax-ppss)) (indique si commentaire ou string)

(require 'sql)
(require 's)
(require 'dash)
(require 'cl)

(defcustom sql-postgres-statement-starters
  (regexp-opt '("declare" "begin" "with" "fetch" "close" "checkpoint"))
  "Additional statement starting keywords in Postgres."
  :version "26.2"
  :type 'string
  :group 'SQL)

(defvar sql-ansi-string-re "'\\(?:''\\|.\\)*'")

(defvar sql-ansi-identifier-string-re "\"\\(?:[\\].\\|.\\)*\"")

(defun esql--patch-sql-alist (product feature value)
  (plist-put (cdr (assoc product sql-product-alist)) feature value))

(esql--patch-sql-alist 'postgres :string-re sql-ansi-string-re)
(esql--patch-sql-alist 'postgres :identifier-string-re sql-ansi-identifier-string-re)
(esql--patch-sql-alist 'postgres :statement sql-postgres-statement-starters)

(sql-set-product-feature 'postgres :syntax-alist '((?\" . "\"")))
(sql-set-product-feature 'postgres :prompt-regexp "^[-[:alnum:]_]*=[#>] ")
(sql-set-product-feature 'postgres :prompt-cont-regexp "^[-[:alnum:]_]*[-(][#>] ")

(defun esql-scratchpad ()
  (interactive)
  (let ((sqli-buffer (call-interactively #'sql-connect))
        (buffer (get-buffer-create (concat "esql[" sql-database "]"))))
    (with-current-buffer buffer
      (setq sql-buffer sqli-buffer)
      (sql-mode)
      (run-hooks 'sql-set-sqli-hook))
    (pop-to-buffer buffer)))

;; Copied from that https://github.com/Fuco1/sql-workbench/blob/master/sql-workbench.el#L226 swb-get-query-bouds-at-point
(defun esql--beginning-of-statement ()
  (save-excursion
    (let ((end (esql--end-of-statement))
          (start (condition-case _err
                     (progn
                       (while (not (and (re-search-backward ";")
                                        (not (esql--inside-comment-or-string-p)))))
                       (1+ (point)))
                   (error (point-min))))
          (continue t))
      (condition-case _err
          (progn
            (goto-char start)
            (while continue
              (setq continue
                    (cond ((looking-at "[ \n\r\f\t]+") (re-search-forward "^[ \n\r\f\t]+") t)
                          ((looking-at "--") (re-search-forward "\n" end) t)
                          ((looking-at "/[*]") (re-search-forward "[*]/" end) t)
                          (t nil))))
            (point))
        (error end)))))


(defun esql--end-of-statement ()
  (save-excursion
    (condition-case _err
        (progn
          (while (not (and (re-search-forward ";")
                           (not (esql--inside-comment-or-string-p)))))
          (point))
      (error (point-max)))))

(defun esql--inside-comment-or-string-p ()
  ;; (declare inline)
  (nth 8 (syntax-ppss)))

;; TODO: use syntax table instead of regexp eg: (nth 8 (syntax-ppss))
(defun esql--extract-query-parameters (start end)
  (save-excursion
    (goto-char start)
    (let ((parameters '())
          ;; avoid comments and strings
          (continue t))
      (while continue
        (if (re-search-forward "\\(--\\)\\|\\(/\\*\\)\\|\\('\\)\\|\\(\"\\)\\|\\([:@][a-zA-Z0-9_-]+\\|\\?\\|\\$[0-9]+\\)" end t)
            (setq continue 
                  (cond 
                   ;; string
                   ((match-string 3)
                    (backward-char 1)
                    (re-search-forward (sql-get-product-feature sql-product :string-re sql-ansi-string-re) end t))
                   ;; identifier
                   ((match-string 4)
                    (backward-char 1)
                    (re-search-forward (sql-get-product-feature sql-product :identifier-string-re sql-ansi-identifier-string-re) end t))
                   ;; single line comment
                   ((match-string 1)
                    (re-search-forward "\n\\|\r\n\\|\r" end t))
                   ;; multiple line comment
                   ((match-string 2)
                    (re-search-forward "\\*/" end t))
                   ;; parameters
                   ((match-string 5)
                    (setq parameters (cons (match-string-no-properties 5) parameters))
                    t)
                   (nil)))
          (setq continue nil)))
      (nreverse parameters))))

;; TODO: support multi line comments
(defun esql--comment-just-before-point ()
  (save-excursion
    (let ((comment-end nil)
          (comment-start nil))
      (if (not (looking-back "\\(--[^\f\n\r]*[\t\n\r ]+\\)" nil))
          nil
        (setq comment-end (match-end 0))
        (setq comment-start (match-beginning 0))
        (goto-char comment-start)
        (cl-loop while (looking-back "\\(--[^\f\n\r]*[\t\n\r ]+\\)" nil)
                 do (progn (setq comment-start (match-beginning 0))
                           (goto-char comment-start)))
        (cons comment-start comment-end)))))

(defun esql--comment-just-after-point ()
  (save-excursion
    (let ((comment-end nil)
          (comment-start nil))
      (if (not (looking-at "\\([\t\n\r ]*--[^\f\n\r]*[\t\n\r ]+\\)"))
          nil
        (setq comment-end (match-end 0))
        (setq comment-start (match-beginning 0))
        (goto-char comment-end)
        (cl-loop while (looking-at "\\(--[^\f\n\r]*[\t\n\r ]+\\)")
                 do (progn (setq comment-end (match-end 0))
                           (goto-char comment-end)))
        (cons comment-start comment-end)))))

;; TODO: refactor to not depend on esql--beginning-of-statement
(defun esql--get-preceding-comment ()
  (save-excursion
    (goto-char (esql--beginning-of-statement))
    (let ((before (esql--comment-just-before-point))
          (after (esql--comment-just-after-point)))
      (if (and after before)
          (cons (car before) (cdr after))
        (or before after)))))

(defun esql--capture-comment-variable ()
  ;; TODO: get value of data accross multiple line
  ;; TODO: support elisp eval eg: -- :email ,gnus-email
  (save-excursion
    (let* ((comment-point (esql--get-preceding-comment))
           (comment-str (buffer-substring-no-properties (car comment-point) (cdr comment-point))))
      (-group-by
       #'car
       (-map
        (lambda (str) 
          (cdr (s-match "\\(:[^ \t]+\\) +\\(.+\\)[ \t]*$" str)))
        (-filter (lambda (str) (s-matches? "^--[ \t]+\\(:[^ \t]+\\) +\\(.+\\)[ \t]*$" str))
                 (s-split "[\n\r]" comment-str)))))))

;; (defun esql--every-parameter-provided ()
;;   "Check that every parameter is provided"

;; ;; eval expr
;; (eval (macroexpand-all
;;        (car (read-from-string "(+ 1 1)"))))

(defun esql--get-parameter (param-alist name)
  (let ((data (cadadr (assoc name param-alist))))
    (if (s-starts-with? "," data)
        (prin1-to-string (eval (macroexpand-all (car (read-from-string (substring data 1))))))
        data)))

;; TODO: support number indexed (questionmark something ?) (the syntaxes: :name, @name, $0, $1 and ?)
;; TODO: sql injection?
(defun esql--replace-parameters (start end param-alist)
  (let* ((text (buffer-substring-no-properties start end))
         (replaces (esql--extract-query-parameters start end)))
    (with-temp-buffer
      (sql-mode)
      (insert text)
      (goto-char 0)
      (while  (and replaces
                   (search-forward (car replaces) end t))
        (let ((replace (car replaces))
              (case-fold-search t)
              (replace-end (point)))
          (when (not (esql--inside-comment-or-string-p))
            (replace-match (esql--get-parameter param-alist replace) t t)
            (setq replaces (cdr replaces)))))
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun esql-send-request ()
  (interactive)
  (save-excursion
    (when (looking-back ";[ \r\n\f\t]*" nil)
      (re-search-backward ";[ \r\n\f\t]*"))
    (let ((start (esql--beginning-of-statement))
          (end (esql--end-of-statement)))
      (sql-send-string (esql--replace-parameters start end (esql--capture-comment-variable))))))

(define-minor-mode esql-minor-mode
  ""
  :lighter "esql"
  :global nil
  :init-value nil
  (if esql-minor-mode
      (progn
        (font-lock-add-keywords
         'sql-mode
         '(("--[ \t]+\\(:[a-zA-Z0-9_-]+\\)" 1 'font-lock-variable-name-face t) ("\\(:[a-zA-Z0-9_-]+\\)" 1 'font-lock-variable-name-face t)))
        ;;(sql-build-completions "public")
        )
    (font-lock-remove-keywords
     'sql-mode
     '(("--[ \t]+\\(:[a-zA-Z0-9_-]+\\)" 1 'font-lock-variable-name-face t) ("\\(:[a-zA-Z0-9_-]+\\)" 1 'font-lock-variable-name-face t)))))

;; TODO: (symbol-value 'sql-completion-object)

;; TODO: (defadvice sql-build-completions

         ;; creer un fichier si aucun n'est créer avec la bonne configuration
       
(provide 'esql)
