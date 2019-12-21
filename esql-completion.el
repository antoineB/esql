;;; ac-sql.el --- auto complete for sql              -*- lexical-binding: t; -*-

;; Copyright (C) 2016  

;; Author:  <antoine@isidore>
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
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; ac-sql

;;; Code:

;; (defun ac-sql--extract-tables ()
;;   (sql-beginning-of-statement (point))
;;   (let ((max
;;          (save-excursion
;;            (sql-end-of-statement (point))
;;            (if (= 0 (point))
;;                (point-max)
;;              (point)))))
;;     (re-search-forward "from\\|FROM" max)
;;     ;; TODO: limit to WHERE|ORDER|LIMIT|HAVING
;;   (re-search-forward "\\([a-zA-Z0-9_]+\\) +AS\\|as +\\([a-zA-Z0-9_]+\\)" max)
;;   (match-string 

(defvar ac-sql-db-definitions nil)

;; (setq ac-sql-db-definitions '(("database_name" . ("table_name0" "table_name1" "table_name2"))))

(load-file "~/.emacs.d/ac-sql-data.el")


(defun ac-sql--source-table-fn ()
  (let ((db (assoc sql-database ac-sql-db-definitions)))
    (if db
        (cdr db)
      '())))

(require 'cl-lib)
(require 'company)
(defun company-ab-sql-table-backend (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-ab-sql-table-backend))
    (prefix (when (looking-back "\\(?:from\\|join\\|FROM\\|JOIN\\)\s+\\(.*\\)")
              (match-string 1)))
    (candidates (all-completions arg (ac-sql--source-table-fn)))))

(defun company-ab-sql-keywords-backend (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-ab-sql-table-backend))
    (prefix (when (looking-back "[^a-zA-Z_-]\\([a-zA-Z]+\\)\\>")
              (match-string 1)))
    (candidates (all-completions arg (list "SELECT"
                                      "FROM"
                                      "JOIN"
                                      "LEFT"
                                      "INNER"
                                      "AS"
                                      "ORDER"
                                      "BY"
                                      "HAVING"
                                      "LIMIT"
                                      "DESC"
                                      "ASC"
                                      "WITH"
                                      "ON"
                                      "WHERE"
                                      "LIKE"
                                      "ILIKE"
                                      "OR"
                                      "AND"
                                      "select"
                                      "from"
                                      "join"
                                      "left"
                                      "inner"
                                      "as"
                                      "order"
                                      "by"
                                      "having"
                                      "limit"
                                      "desc"
                                      "asc"
                                      "with"
                                      "on"
                                      "where"
                                      "like"
                                      "ilike"
                                      "or"
                                      "and")))))

(defun company-ab-sql-type-backend (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-ab-sql-table-backend))
    (prefix (when (looking-back "::\\([a-zA-Z]+\\)\\>")
              (match-string 1)))
    (candidates (all-completions arg (list "int4range"
                                           "int8range"
                                           "numrange"
                                           "tsrange"
                                           "tstzrange"
                                           "daterange"
                                           "any"
                                           "anyarray"
                                           "anyelement"
                                           "anyenum"
                                           "anynonarray"
                                           "cstring"
                                           "internal"
                                           "language_handler"
                                           "fdw_handler"
                                           "record"
                                           "trigger"
                                           "void"
                                           "opaque"
                                           "bit"
                                           "bit()"
                                           "bit varying"
                                           "bit varying()"
                                           "bigint"
                                           "int8"
                                           "bigserial"
                                           "serial8"
                                           "boolean"
                                           "bool"
                                           "box"
                                           "bytea"
                                           "character"
                                           "character()"
                                           "char"
                                           "char()"
                                           "character varying"
                                           "character varying()"
                                           "varchar()"
                                           "varchar"
                                           "cidr"
                                           "circle"
                                           "date"
                                           "double precision"
                                           "float8"
                                           "inet"
                                           "integer"
                                           "int"
                                           "int4"
                                           ;; TODO interval [ fields ] [ (p) ]
                                           "json"
                                           "jsonb"
                                           "line"
                                           "lseg"
                                           "macaddr"
                                           "money"
                                           "numeric"
                                           "numeric(p,s)"
                                           "decimal"
                                           "decimal(p,s)"
                                           "path"
                                           "pg_lsn"
                                           "point"
                                           "polygon"
                                           "real"
                                           "float4"
                                           "smallint"
                                           "int2"
                                           "smallserial"
                                           "serial2"
                                           "serial"
                                           "serial4"
                                           "text"
                                           "time"
                                           "time without time zone"
                                           "time with time zone"
                                           "time(0) without time zone"
                                           "time(0) with time zone"
                                           "timestamp without time zone"
                                           "timestamp with time zone"
                                           "timestamp(0) without time zone"
                                           "timestamp(0) with time zone"
                                           "timetz"
                                           "timestamptz"
                                           "tsquery"
                                           "tsvector"
                                           "txid_snapshot"
                                           "uuid"
                                           "xml"
                                           ;; TODO rajouter "[]" a tous les types?
                                           )))))
