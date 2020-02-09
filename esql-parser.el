;;; esql-parser.el --- Minimal SQL parser            -*- lexical-binding: t; -*-

;; Copyright (C) 2020  

;; Author:  <antoine@antoine-AB350-Gaming>
;; Keywords: languages

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

;; automatique reconnection ? (memq (process-status proc) '(stop))


;; (defvar js2-parse-interruptable-p t
;;   "Set this to nil to force parse to continue until finished.
;; This will mostly be useful for interpreters.")

;; (defvar js2-statements-per-pause 50
;;   "Pause after this many statements to check for user input.
;; If user input is pending, stop the parse and discard the tree.
;; This makes for a smoother user experience for large files.
;; You may have to wait a second or two before the highlighting
;; and error-reporting appear, but you can always type ahead if
;; you wish.  This appears to be more or less how Eclipse, IntelliJ
;; and other editors work.")


(require 'cl-lib)
(require 's)

(defvar esql-parser-EOF 1)
(defvar esql-parser-NUMBER 2)
(defvar esql-parser-STRING 3)
(defvar esql-parser-WHITESPACE 4)
(defvar esql-parser-ERROR 5)
(defvar esql-parser-BALANCED-BRACKET 6)
(defvar esql-parser-BALANCED-PAREN 7)
(defvar esql-parser-PUNCTUATION 8)
(defvar esql-parser-SEMICOLON 9)
(defvar esql-parser-IDENTIFIER 10)

(cl-defstruct (esql-parser-token)
  type
  beg
  end
  str)

;; (make-esql-parser-token :type 1 :beg 2 :end 3 :str 4)

(cl-defstruct (esql-parser-lexer-state)
  tokens
  lineno
  point)

(defvar-local esql-parser-lex-state nil)

(defun esql-parser-read-number ()
  (when (looking-at "\\([0-9]+[.][0-9]+\\|[.][0-9]+\\|[0-9]+\\)")
    (goto-char (+ (point) (length (match-string 0))))
    (make-esql-parser-token
     :type esql-parser-NUMBER
     :beg (point)
     :end (length (match-string 0))
     :str (match-string-no-properties 0))))

(defun esql-parser-read-string ()
  (when (looking-at "'\\(?:''\\|.\\)*'")
    (goto-char (+ (point) (length (match-string 0))))
    (make-esql-parser-token
     :type esql-parser-STRING
     :beg (point)
     :end (length (match-string 0))
     :str (match-string-no-properties 0))))
  
(defun esql-parser-read-litteral ()
  (or (esql-parser-read-number)
      (esql-parser-read-string)))

(defun esql-parser-read-whitespace ()
  (when (looking-at "[ \t\f\r\n]+")
    (goto-char (+ (point) (length (match-string 0))))
    (make-esql-parser-token
     :type esql-parser-WHITESPACE
     :beg (point)
     :end (length (match-string 0))
     :str (match-string-no-properties 0))))

(defun esql-parser-read-balanced-bracket ()
  (when (looking-at "\\[")
    (let ((start (point)))
      (condition-case _err
          (progn
            (forward-sexp)
            (make-esql-parser-token
             :type esql-parser-BALANCED-BRACKET
             :beg start
             :end (point)
             :str (buffer-substring-no-properties start (point))))
        (error (progn
                 (goto-char (point-max))
                 (make-esql-parser-token
                  :type esql-parser-ERROR
                  :beg start
                  :end (point-max)
                  :str (buffer-substring-no-properties start (point-max)))))))))

(defun esql-parser-read-balanced-paren ()
  (when (looking-at "(")
    (let ((start (point)))
      (condition-case _err
          (progn
            (forward-sexp)
            (make-esql-parser-token
             :type esql-parser-BALANCED-PAREN
             :beg start
             :end (point)
             :str (buffer-substring-no-properties start (point))))
        (error (progn
                 (goto-char (point-max))
                 (make-esql-parser-token
                  :type esql-parser-ERROR
                  :beg start
                  :end (point-max)
                  :str (buffer-substring-no-properties start (point-max)))))))))


;; (defun esql-parser-read-binary-op ()
;; (defun esql-parser-read-unary-op ()
;; (defun esql-parser-read-type ()

      
(defun esql-parser-read-identifier ()
  (cond
   ((looking-at "\"\\(?:[\\].\\|.\\)*\"")
    (goto-char (+ (point) (length (match-string 0))))
    (make-esql-parser-token
     :type esql-parser-IDENTIFIER
     :beg (point)
     :end (length (match-string 0))
     :str (match-string-no-properties 0)))
   ((looking-at "[a-zA-Z_][a-zA-Z_0-9]*")
    (goto-char (+ (point) (length (match-string 0))))
    (make-esql-parser-token
     :type esql-parser-IDENTIFIER
     :beg (point)
     :end (length (match-string 0))
     :str (match-string-no-properties 0)))
   (:else nil)))
  
(defun esql-parser-read-punctuation ()
  (when (looking-at "[.,]")
    (goto-char (+ (point) (length (match-string 0))))
    (make-esql-parser-token
     :type esql-parser-PUNCTUATION
     :beg (point)
     :end (length (match-string 0))
     :str (match-string-no-properties 0))))

(defun esql-parser-read-semicolon ()
  (when (looking-at ";")
    (forward-char)
    (make-esql-parser-token
     :type esql-parser-SEMICOLON
     :beg (- (point) 1)
     :end (point)
     :str ";")))

(defun esql-parser-read-error ()
  (forward-char)
  (make-esql-parser-token
   :type esql-parser-ERROR
   :beg (- (point) 1)
   :end (point)
   :str (string (char-before))))

(defun esql-parser-read-eof ()
  (when (eq (point) (point-max))
    (make-esql-parser-token
     :type esql-parser-EOF
     :beg (point)
     :end (point)
     :str "")))

(defun esql-parser-next-token ()
  (let ((tok (or
              (esql-parser-read-eof)
              (esql-parser-read-semicolon)
              (esql-parser-read-whitespace)
              (esql-parser-read-litteral)

              ;; (esql-parser-read-binary-op)
              ;; (esql-parser-read-unary-op)
              ;; (esql-parser-read-type)
              
              (esql-parser-read-balanced-paren)
              (esql-parser-read-balanced-bracket)
              (esql-parser-read-punctuation)
              (esql-parser-read-identifier)
              
              (esql-parser-read-error))))
    (when tok
      (setf (esql-parser-lexer-state-tokens esql-parser-lex-state)
            (cons tok (esql-parser-lexer-state-tokens esql-parser-lex-state)))
      (setf (esql-parser-lexer-state-point esql-parser-lex-state) (point))
      (when (memq (esql-parser-token-type tok) (list esql-parser-WHITESPACE
                                                     esql-parser-BALANCED-BRACKET
                                                     esql-parser-BALANCED-PAREN
                                                     esql-parser-IDENTIFIER))
        (let* ((token-str (esql-parser-token-str tok))
               (positions (s-matched-positions-all "[\r][\n]\\|[\r]\\|[\n]" token-str)))
          (when positions
            (setf (esql-parser-lexer-state-lineno esql-parser-lex-state)
                  (+ (length positions) (esql-parser-lexer-state-lineno esql-parser-lex-state))))))
      tok)))

(defun esql-parser-init-lexing ()
  "Set or re-set the lexing state, like moving point to previous position"
  (when (not esql-parser-lex-state)
    (setq esql-parser-lex-state (make-esql-parser-lexer-state :tokens '() :lineno 1 :point (min-point))))
  (goto-char (esql-parser-lexer-state-point esql-parser-lex-state)))

(defun esql-parser-init-lexing (&optional restart)
  "Set or re-set the lexing state, like moving point to previous position"
  (when (or (not esql-parser-lex-state) restart)
    (setq esql-parser-lex-state (make-esql-parser-lexer-state :tokens '() :lineno 1 :point (point-min))))
  (goto-char (esql-parser-lexer-state-point esql-parser-lex-state)))

(defun esql-parser-lex-whole-buffer ()
  (save-excursion
    (esql-parser-init-lexing t)
    (let ((tok (esql-parser-next-token)))
      (while (and tok (not (eq (esql-parser-token-type tok) esql-parser-EOF)))
        (setq tok (esql-parser-next-token))))
    (reverse (esql-parser-lexer-state-tokens esql-parser-lex-state))))

;; TODO: threads ?
(require 'threads)
(defun esql-parser-th-lex-whole-buffer ()
  (make-thread (lambda ()
                 (let (tok)
                   (save-excursion
                     (esql-parser-init-lexing t)
                     (setq tok (esql-parser-next-token)))
                   (while (and tok (not (eq (esql-parser-token-type tok) esql-parser-EOF)))
                     (thread-yield)
                     (save-excursion
                       (setq tok (esql-parser-next-token)))))
                 ;; TODO: do something
                 )))

;; TODO: after-change-functions before-change-functions
;; post-self-insert-hook

(provide 'esql-parser)
;;; esql-parser.el ends here
