;;; esql.el ---                                      -*- lexical-binding: t; -*-

;; Copyright (C) 2019  

;; Author:  <antoine597@gmail.com>
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

(require 'sql)
(require 's)
(require 'dash)
(require 'cl)
(require 'cl-lib)
(require 'company)

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

(sql-set-product-feature 'ansi :default-schema "")
(sql-set-product-feature 'ansi :keywords
                         '("select" "from" "join" "left" "inner" "as" "order" "by"
                           "having" "limit" "desc" "asc" "with" "on" "where" "like"
                           "ilike" "or" "and"))

(sql-set-product-feature 'ansi :datatypes '("array" "binary" "bit" "blob" "boolean" "char" "character" "clob"
                                            "date" "dec" "decimal" "double" "float" "int" "integer" "interval"
                                            "large" "national" "nchar" "nclob" "numeric" "object" "precision"
                                            "real" "ref" "row" "scope" "smallint" "time" "timestamp" "varchar"
                                            "varying" "zone"))

;; TODO add "[]" to all types?
(sql-set-product-feature 'postgres :datatypes '("int4range" "int8range" "numrange" "tsrange" "tstzrange" "daterange"
                                                "any" "anyarray" "anyelement" "anyenum" "anynonarray" "cstring" "internal"
                                                "language_handler" "fdw_handler" "record" "trigger" "void" "opaque"
                                                "bit" "bit()" "bit varying" "bit varying()" "bigint" "int8" "bigserial"
                                                "serial8" "boolean" "bool" "box" "bytea" "character" "character()"
                                                "char" "char()" "character varying" "character varying()" "varchar()"
                                                "varchar" "cidr" "circle" "date" "double precision" "float8" "inet"
                                                "integer" "int" "int4"
                                                ;; TODO interval [ fields ] [ (p) ]
                                                "json" "jsonb" "line" "lseg" "macaddr" "money" "numeric" "numeric(p,s)" 
                                                "decimal" "decimal(p,s)" "path" "pg_lsn" "point" "polygon" "real" "float4"
                                                "smallint" "int2" "smallserial" "serial2" "serial" "serial4" "text" "time"
                                                "time without time zone" "time with time zone" "time(0) without time zone"
                                                "time(0) with time zone" "timestamp without time zone" "timestamp with time zone"
                                                "timestamp(0) without time zone" "timestamp(0) with time zone" "timetz"
                                                "timestamptz" "tsquery" "tsvector" "txid_snapshot" "uuid" "xml"))

(sql-set-product-feature 'ansi :functions '())

(defun esql-scratchpad ()
  (interactive)
  (let ((sqli-buffer (call-interactively #'sql-connect))
        (buffer (get-buffer-create (concat "esql[" sql-database "]"))))
    (with-current-buffer buffer
      (setq sql-buffer sqli-buffer)
      (sql-mode)
      (run-hooks 'sql-set-sqli-hook)
      (esql-minor-mode t))
    (pop-to-buffer buffer)))

;; Copied from https://github.com/Fuco1/sql-workbench/blob/master/sql-workbench.el#L226 swb-get-query-bouds-at-point
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
  (save-excursion
    (let* ((comment-point (esql--get-preceding-comment)))
      (when comment-point
        (let ((comment-str (buffer-substring-no-properties (car comment-point) (cdr comment-point))))
          (-group-by
           #'car
           (-map
            (lambda (str) 
              (cdr (s-match "\\(:[^ \t]+\\) +\\(.+\\)[ \t]*$" str)))
            (-filter (lambda (str) (s-matches? "^--[ \t]+\\(:[^ \t]+\\) +\\(.+\\)[ \t]*$" str))
                     (s-split "[\n\r]" comment-str)))))))))


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
        ;; TODO: place it here instead of company completion (esql-build-completions "public")
        (local-key-binding (kbd "C-c C-c") 'esql-send-request))
    (font-lock-remove-keywords
     'sql-mode
     '(("--[ \t]+\\(:[a-zA-Z0-9_-]+\\)" 1 'font-lock-variable-name-face t) ("\\(:[a-zA-Z0-9_-]+\\)" 1 'font-lock-variable-name-face t)))))


;; Completions
;; TODO: Do the schema
;; TODO: (symbol-value 'sql-completion-object)

;; Return list of string
(defun esql-postgres-completion-table (sqlbuf schema)
  (sql-redirect sqlbuf "\\a")
  (let ((tables (sql-redirect-value sqlbuf "\\dt" (concat schema "|\\([^|]+\\)") 1)))
    (sql-redirect sqlbuf "\\a")
    tables))

(defun esql--group-by-table (table-columns)
  (let ((result '()))
    (dolist (elem table-columns)
      (if (assoc (car elem) result)
          (setcdr (assoc (car elem) result) (append (cdr (assoc (car elem) result)) (cdr elem)))
        (add-to-list 'result `(,(car elem) . ,(cdr elem)))))
    result))

(defun esql-postgres-completion-column (sqlbuf schema)
  (let ((query (format "SELECT table_schema, table_name, column_name
                        FROM information_schema.columns
                        WHERE table_schema = '%s'
                        ORDER BY table_schema, table_name, column_name;" (esql-quote-string schema)))
        result)
    (sql-redirect sqlbuf "\\a")
    (setq result (esql--group-by-table (sql-redirect-value sqlbuf query (concat "^" schema "|\\([^|]+\\)|\\(.+\\)$") '(1 2))))
    (sql-redirect sqlbuf "\\a")
    result))

(sql-set-product-feature 'postgres :default-schema "public")
(sql-set-product-feature 'postgres :completion-table #'esql-postgres-completion-table)
(sql-set-product-feature 'postgres :completion-table-column #'esql-postgres-completion-column)


(defun esql-sqlite-completion-table (sqlbuf schema)
  (sql-redirect-value sqlbuf
                      "SELECT name FROM sqlite_master WHERE type = 'table';"
                      ".+"))

(defun esql-sqlite-completion-column (sqlbuf schema)
  (sql-redirect sqlbuf
                "SELECT name, sql FROM sqlite_master WHERE type = 'table';"
                "*esql-result*")
  (let ((tables '()))
    (while (not (eq (point) (point-max)))
      (let ((end-line (save-excursion (move-end-of-line 1) (point)))
            table-name
            columns)
        (and (re-search-forward "CREATE TABLE \"?\\([^(\"?]+\\)\"?" end-line t)
             (progn
               (setq table-name (match-string-no-properties 1))
               (forward-char)
               (let ((loop t))
                 (while (and loop (re-search-forward "[^ ]+" end-line t))
                   (setq columns (append columns (list (match-string-no-properties 0))))
                   (setq loop (re-search-forward "[,(]" end-line t))
                   (when (eq (char-before) ?\()
                     (backward-char)
                     (forward-sexp)
                     (forward-char 2))))))
        (goto-char end-line)
        (forward-char)
        (add-to-list 'tables `(,table-name . ,columns))))
    (kill-buffer "*esql-result*")
    tables))

(sql-set-product-feature 'sqlite :default-schema "")
(sql-set-product-feature 'sqlite :completion-table #'esql-sqlite-completion-table)
(sql-set-product-feature 'sqlite :completion-table-column #'esql-sqlite-completion-column)

(defun esql-mysql-completion-table (sqlbuf schema)
  ;; (sql-redirect sqlbuf "\\n")
  (let ((result (-map #'s-trim (-drop 1 (sql-redirect-value sqlbuf
                                             "SHOW TABLES;"
                                             "^| \\(.+\\) +|$" 1)))))
    ;; (sql-redirect sqlbuf "\\P")
    result))

(defun esql-quote-string (str)
  (s-replace "'" "''" str))

(defun esql-mysql-completion-column (sqlbuf schema)
  ;; Make the query on one line otherwise no data is returned by sql-redirect-value.
  (let ((query (format "SELECT table_schema, table_name, column_name FROM information_schema.columns WHERE table_schema = '%s' ORDER BY table_schema, table_name,column_name;" (esql-quote-string schema))))
    (esql--group-by-table
     (-map (lambda (lst) (list (s-trim (car lst)) (s-trim (cadr lst))))
           ;; TODO: Doesn't work name likes `foo|bar`
           (-drop 1 (sql-redirect-value sqlbuf query "^|[^|]+|\\([^|]+\\)|\\([^|]+\\)|$" '(1 2)))))))

(defun esql-mysql-completion-column-bis (sqlbuf schema)
  (let* ((query (format "SELECT table_schema, table_name, column_name FROM information_schema.columns WHERE table_schema = '%s' ORDER BY table_schema, table_name,column_name;" (esql-quote-string schema)))
         (result (sql-redirect-value sqlbuf query ".+" 0))
         (cells (cdr (s-match "[+]\\(-+\\)[+]\\(-+\\)[+]\\(-+\\)" (car result))))
         (result- (-drop 3 (-take (- (length result) 2) result))))
    (esql--group-by-table
     (-map (lambda (line)
             (let* ((cells0 (+ 1 (length (car cells))))
                    (cells1 (+ cells0 1 (length (cadr cells))))
                    (cells2 (+ cells1 1 (length (caddr cells)))))
               (list (s-trim (substring line (+ 1 cells0) cells1))
                     (s-trim (substring line (+ 1 cells1) cells2)))))
           result-))))

(sql-set-product-feature 'mysql :default-schema 'sql-database)
(sql-set-product-feature 'mysql :completion-table #'esql-mysql-completion-table)
(sql-set-product-feature 'mysql :completion-table-column #'esql-mysql-completion-column-bis)

(sql-set-product-feature 'mysql :prompt-regexp "^[^>]+> ")

(defun esql-build-completions-1 (schema completion-list feature)
  (let ((f (sql-get-product-feature sql-product feature)))
    (when (and f sql-buffer)
      (set completion-list (funcall f (get-buffer sql-buffer) schema)))))

(defun esql-build-completions (schema)
  (interactive)
  (when (not (condition-case _err (symbol-value 'sql-built) (error nil)))
    (set 'sql-completion-table nil)
    (set 'sql-completion-table-column nil)
    (esql-build-completions-1 schema 'sql-completion-table :completion-table)
    (esql-build-completions-1 schema 'sql-completion-table-column :completion-table-column)
    (set 'sql-built t)))

(defun esql-company-table-backend (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'esql-company-table-backend))
    (prefix (when (looking-back "\\(?:from\\|join\\|FROM\\|JOIN\\)\s+\\(.*\\)")
              (match-string 1)))
    (candidates (progn (esql-build-completions (sql-get-product-feature sql-product :default-schema 'ansi)) (all-completions arg (symbol-value 'sql-completion-table))))))
;; TODO: support multiple schema
;; TODO: support table name with quote like "some table name"

(defun esql-company-keywords-backend (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'esql-company-keywords-backend))
    (prefix (when (looking-back "[^a-zA-Z_-]\\([a-zA-Z]+\\)\\>")
              (match-string 1)))
    (candidates (all-completions arg (sql-get-product-feature sql-product :keywords 'ansi)))))

(defun esql-company-datatypes-backend (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'esql-company-datatypes-backend))
    (prefix (when (looking-back "::\\([a-zA-Z]+\\)\\>")
              (match-string 1)))
    (candidates (all-completions arg (sql-get-product-feature sql-product :datatypes 'ansi)))))

(defun esql-s-trim-string (surrounding str)
  (if (and (s-starts-with? surrounding str)
             (s-ends-with? surrounding str))
      (substring str (length surrounding) (- (length str) (length surrounding)))
    str))

(defun esql-table-alias ()
  (let ((start (esql--beginning-of-statement))
        (end (esql--end-of-statement))
        (case-fold-search t)
        (result '()))
    (save-excursion
      (goto-char start)
      ;; TODO: schema
      (while (re-search-forward "\\(from\\|join\\)[ \t\r\n]+\\([^ \t\r\n]+\\)\\([ \t\r\n]+as[ \t\r\n]+\\([^ \t\r\n]+\\)\\)?" end t)
        (when (not (esql--inside-comment-or-string-p))
          (add-to-list 'result (cons (or (match-string-no-properties 4) (esql-s-trim-string "\"" (match-string-no-properties 2)))
                                     (esql-s-trim-string "\"" (match-string-no-properties 2))))))
      result)))

;; TODO: make a backend when no alias is provided
(defun esql-company-column-backend (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'esql-company-column-backend))
    (prefix (when (looking-back "\\([a-zA-Z_0-9]+\\)\\([.]\\([a-zA-Z_0-9]*\\)\\)")
              (match-string-no-properties 0)))
    (candidates ;;(all-completions arg (sql-get-product-feature sql-product :datatypes 'ansi))
     (progn (esql-build-completions (sql-get-product-feature sql-product :default-schema 'ansi))
            (let ((alias (esql-table-alias))
                  (arg- (car (s-split "\\." arg))))
              (all-completions arg (-map (lambda (x) (concat arg- "." x))
                                          (cdr (assoc (cdr (assoc arg- alias)) (symbol-value 'sql-completion-table-column)))))))
         
     )))

(defun esql-hook-fn ()
  (require 'company)
  (company-mode t)
  (make-local-variable 'company-backends)
  ;; TODO: keyword backend does work
  (setq company-backends '((esql-company-column-backend esql-company-keywords-backend esql-company-table-backend esql-company-datatypes-backend company-dabbrev))))

(add-hook 'sql-interactive-mode-hook #'esql-hook-fn)
(add-hook 'esql-minor-mode-hook #'esql-hook-fn)

(provide 'esql)
