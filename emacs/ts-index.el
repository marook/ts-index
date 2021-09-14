;; -*- lexical-binding: t -*-
;;
;; ts-index - a fast typescript artifact index
;; Copyright (C) 2021  Markus Peröbner
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Affero General Public License for more details.
;;
;; You should have received a copy of the GNU Affero General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

(defun ts-index--goto-global-artifact (candidates)
  (let ((candidate (car candidates)))
    (seq-let (file-path type name exported point) candidate
      (find-file file-path)
      (goto-char (+ point 1))
      )))

(defun ts-index-relative-import (candidate)
  "ts-index-relative-import returns an import statement string for the given candidate."
  (seq-let (file-path type name exported point) candidate
    (concat
     "import { "
     name
     " } from '"
     ;; TODO quote ' in file name
     ;; TODO use / in import path also on windows
     (let ((file-import-path (string-remove-suffix ".ts" (file-relative-name file-path))))
       (unless (string-prefix-p "." file-import-path)
         (setq file-import-path (concat "./" file-import-path)))
       file-import-path)
     "';\n"
     )))

(defvar ts-index-import-hook
  `(
    ts-index-relative-import
    )
  "ts-index-import-hook contains functions which convert candidates into import statement strings.

The first function which returns not nil wins.

Each function is called with one candidate argument. A candidate
is a list containing (file-path type name exported point).")

(defun ts-index--insert-import (candidates)
  (mapc
   (lambda (candidate)
     (let ((statement (run-hook-with-args-until-success
                       'ts-index-import-hook
                       candidate)))
       (when statement (insert statement))
      ))
   candidates))

(defun ts-index--global-artifacts-source (project-name project-buffer-name)
  (helm-build-sync-source (concat project-name " artifacts")
    :candidates
    (lambda ()
      (with-current-buffer project-buffer-name
        ts-index-global-artifacts))
    :volatile t
    :candidate-number-limit 999
    :candidate-transformer
    (lambda (candidates)
      (mapcar
       (lambda (artifact)
         (seq-let (file-path type name exported point) artifact
           (let ((line (concat (substring type 0 1) (if exported "e" "-") " " name " " (file-name-nondirectory file-path))))
             (put-text-property 0 2 'face 'shadow line)
             (put-text-property (+ 3 (length name) 1) (length line) 'face 'shadow line)
             (list line artifact)
             )))
       candidates))
    :action '(
              ("Goto" . ts-index--goto-global-artifact)
              ("Import" . ts-index--insert-import)
              )))

(defun ts-index--find-in-project (project-name project-buffer-name)
  (helm :sources (ts-index--global-artifacts-source project-name project-buffer-name)))

(defun ts-index--merge-add-global-artifact (buffer-name change-args)
  (seq-let (file-path type name exported point) change-args
    (with-current-buffer buffer-name
      (setq-local ts-index-global-artifacts
                  (append ts-index-global-artifacts `(,(list file-path type name exported point)))))))

(defun ts-index--merge-remove-global-artifact (buffer-name change-args)
  (seq-let (file-path) change-args
    (with-current-buffer buffer-name
      (setq-local ts-index-global-artifacts
                  (seq-filter
                   (lambda (artifact)
                     (seq-let (a-file-path) artifact
                       (not (string= file-path a-file-path))))
                   ts-index-global-artifacts)))))

(defun ts-index--merge-change-into-index (buffer-name change)
  (with-current-buffer buffer-name
    (let ((change-type (car change)) (change-args (cdr change)))
      (cond
       ((string= change-type "+") (ts-index--merge-add-global-artifact buffer-name change-args))
       ((string= change-type "-") (ts-index--merge-remove-global-artifact buffer-name change-args))
       (t (message "Unknown ts-index change type occured: %s" change-type))))))

(defvar ts-index-watch-cmd
  '("ts-index")
  "ts-index-watch-cmd is a list of command line arguments executed when starting to watch a project using the ts-index node.js application.

The project's root directory is automatically added to the
arguments when run.

A usual declaration where node.js is executed directly might look
like this:

'(\"node\" \"/path/to/ts-index\"))")

(defun ts-index--create-project-buffer (project-root project-name project-buffer-name)
  (make-process
   :name (s-concat project-name " ts-index watcher")
   :buffer project-buffer-name
   :command (append ts-index-watch-cmd `(,(expand-file-name project-root)))
   :noquery t
   :filter
   (lambda (p s)
     (with-current-buffer project-buffer-name
       (setq-local inhibit-read-only t)
       (mapc
        (lambda (expr)
          (if (and
               (string-prefix-p "(" expr)
               (string-suffix-p ")" expr))
              ;; TODO the read expr is probably an attack vector against the running emacs instance?
              (ts-index--merge-change-into-index project-buffer-name (read expr))
            (insert (s-concat expr "\n"))
            ))
        (split-string s "\n"))
       (setq-local inhibit-read-only nil))))
  (with-current-buffer project-buffer-name
    (setq-local ts-index-project-root project-root)
    (setq-local ts-index-project-name project-name)
    (setq-local ts-index-global-artifacts ())
    (read-only-mode))
  (get-buffer project-buffer-name))

;; (ts-index-find)
(defun ts-index-find ()
  "`ts-index-find` shows global typescript artifacts of the current project.

The helm candidates look like this:
ce MyClass my-class.ts

The first character indicates the artifact type. Right now the
following types are possible:

c - class
e - enum
f - function
i - interface
m - module
t - type
v - variable

The second character indicates if the artifact is exported via an
e character. - indicates it is not exported.

The status characters are followed by the name of the artifact in
a second column.

The file name in where the artifact was found is the third
column.

Invoking this function will create a buffer with a name like
'*<projectname> ts-index*' which launches a ts-index process in
order to scan the project's files. Kill the buffer if you don't
need the index anymore.
"
  (interactive)
  (let (project-root project-name project-buffer-name project-buffer)
    (setq project-root (elpy-project-root))
    (if project-root
        (progn
          (setq project-name (ts-index--project-name project-root))
          (setq project-buffer-name (s-concat "*" project-name " ts watcher*"))
          (setq project-buffer (get-buffer project-buffer-name))
          (unless project-buffer
            (setq project-buffer (ts-index--create-project-buffer project-root project-name project-buffer-name)))
          (ts-index--find-in-project project-name project-buffer-name))
      (message "Could not determine a project directory for ts-index."))))

;; (ts-index--project-name "~/projects/my-project/")
(defun ts-index--project-name (project-path)
  (file-name-nondirectory (directory-file-name project-path)))

(provide 'ts-index)
