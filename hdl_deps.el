;;; package -- hdl-deps
;;;
;;; Commentary:
;;;
;;; Code:
;;;

(require 'projectile)

(defun hdldep--graph-current-buffer (dir)
  ""
  (interactive (list (read-directory-name "Search Directory:" (projectile-project-root))))
  (print dir)
  (print (type-of dir))

  (let* ((file (buffer-file-name))
         (entity (hdldep--vhdl-get-entity-name file)))
    (with-temp-file
        (format "%s.gv" (file-name-base file))
      (insert
       (hdldep--create-digraph-for-module dir
                                          (symbol-name entity))))))

(defun hdldep--create-digraph-for-module (dir module)
  ""
  (hdldep--create-digraph
   (delete-dups
    (hdldep--find-connected-edges
     (hdldep--get-all-edges
      (hdldep--get-all-hdl-files dir))
     (list) (intern module)))))

(defun hdldep--create-digraph (edges)
  ""
  (let ((s ""))
    (setq s (concat s "graph {\n"))
    (dolist (edge edges)
      (setq s (concat s (format "   %s -- %s \n" (car edge) (cdr edge)))))
    (setq s (concat s "}\n"))))

(defun hdldep--find-connected-edges (all-edges connected-edges node)
  ""
  (dolist (edge all-edges)
    (when (equal (car edge) node)
      (let ((start (car edge))
            (end (cdr edge)))

        ;; this is the target node; concat it to the list of connected
        ;; notes and then remove it
        (setq connected-edges (append (list edge) connected-edges))
        (setq all-edges (remove edge all-edges))

        ;; look for child nodes
        (setq connected-edges
              (hdldep--find-connected-edges all-edges connected-edges end)))))
  connected-edges)

(defun hdldep--get-all-hdl-files (dir)
  ""
  (split-string (shell-command-to-string
                 (format (concat
                          "find %s \\("
                          " -name \"*.vhdl\"" " -o"
                          " -name \"*.vhd\""  " -o"
                          " -name \"*.v\""    " -o"
                          " -name \"*.sv\" "
                          "\\)")
                         dir))))

(defun hdldep--get-all-edges (files)
  ""
  (let ((all-edges nil))
    (dolist (file files)
      (princ (format "Getting edges for %s...\n" file))
      ;; FIXME: verilog vs. vhdl
      (let ((edges (hdldep--deps-list-to-edges
                    (hdldep--vhdl-make-deps-list file))))
        (if edges
            (setf all-edges (append edges all-edges)))
        )) (delete-dups all-edges)))

(defun hdldep--deps-list-to-edges (deps-list)
  "convert a dependency list deps-list into dot lines.
car of the list is the module
cdr of the list is the dependencies"
  (let ((edges nil))
    (dolist (dep (cdr deps-list))
      (push (cons (car deps-list) dep) edges)
      )
    edges))

;;------------------------------------------------------------------------------
;; Language Agnostic Wrapper
;;------------------------------------------------------------------------------

(defun hdldep--file-is-verilog (file)
  ""
  (or (string= (file-name-extension file) ".v")
      (string= (file-name-extension file) ".sv")))

(defun hdldep--file-is-vhdl (file)
  ""
  (or (string= (file-name-extension file) ".vhd")
      (string= (file-name-extension file) ".vhdl")))

(defun hdldep--vhdl-or-verilog (file func-vhdl func-verilog)
  ""
  (cond
   ((hdldep--file-is-vhdl file)
    (funcall func-vhdl file))
   ((hdldep--file-is-verilog file)
    (funcall func-verilog file))
   t))

(defun hdldep--make-deps-list (file)
  ""
  (hdldep--vhdl-or-verilog
   file
   #'hdldep--verilog-make-deps-list
   #'hdldep--vhdl-make-deps-list))

(defun hdldep--get-entity-name (file)
  ""
  (hdldep--vhdl-or-verilog
   file
   #'hdldep--verilog-get-entity-name
   #'hdldep--vhdl-get-entity-name))

(defun hdldep--get-entities (file)
  ""
  (hdldep--vhdl-or-verilog
   file
   #'hdldep--verilog-get-entities
   #'hdldep--vhdl-get-entities))

;;------------------------------------------------------------------------------
;; VERILOG
;;------------------------------------------------------------------------------

;; FIXME: make the vhdl and verilog common... put them in a macro..?

(defun hdldep--verilog-flatten-buffer ()
  "Flatten a Verilog buffer.
removes all comments and newlines for
easier processing as a stream."
  ;; remove all comments
  (while (re-search-forward "\/\/.*\n" nil t)
    (replace-match ""))
  (goto-char (point-min))

  ;; with comments removed, safe to remove all newlines
  (while (re-search-forward "\n" nil t)
    (replace-match " "))
  (goto-char (point-min)))

(defun hdldep--verilog-make-deps-list (file)
  ""
  (append
   (list (hdldep--verilog-get-entity-name file))
   (hdldep--verilog-get-entities file)))

(defun hdldep--verilog-get-entity-name (file)
  ""
  (with-temp-buffer
    (insert-file-contents file)
    (hdldep--verilog-flatten-buffer)
    (while (re-search-forward
            (concat
             "entity" ;; instance name
             "\s+"
             "\\([A-z,0-9]+\\)" ;; library
             "\s+"
             "is"
             ) nil t 1))
    (let ((match (match-string 1)))
      ;; if there is a match, intern it, otherwise return nil
      (if match (intern match) nil))))

(defun hdldep--verilog-get-entities (file)
  "Get all entities in VERILOG file FILE."
  (require 'cl)
  (let ((entities nil))
    (with-temp-buffer
      (insert-file-contents file)
      (hdldep--verilog-flatten-buffer)
      (while (re-search-forward
              (concat
               "\\(" "[A-z,0-9]+" "\\)" ;; instance name
               "\s+" ":" "\s+" "entity" "\s+" ;;
               "\\(" "[A-z,0-9]+" "\\)" "\." ;; library
               "\\(" "[A-z,0-9]+" "\\)" ;; entity
               ) nil t)
        (let ((match (match-string 3)))
          (when match
            (push (intern match) entities)
            ))))
    (delete-dups entities)))

;;------------------------------------------------------------------------------
;; VHDL
;;------------------------------------------------------------------------------

(defun hdldep--vhdl-flatten-buffer ()
  "Flatten a VHDL buffer.
removes all comments and newlines for
easier processing as a stream."
  ;; remove all comments
  (while (re-search-forward "--.*\n" nil t)
    (replace-match ""))
  (goto-char (point-min))

  ;; with comments removed, safe to remove all newlines
  (while (re-search-forward "\n" nil t)
    (replace-match " "))
  (goto-char (point-min)))

;; FIXME this could be made directly language agnostic
(defun hdldep--vhdl-make-deps-list (file)
  ""
  (append
   (list (hdldep--vhdl-get-entity-name file))
   (hdldep--vhdl-get-entities file)))

(defun hdldep--vhdl-get-entity-name (file)
  ""
  (with-temp-buffer
    (insert-file-contents file)
    (hdldep--vhdl-flatten-buffer)
    (while (re-search-forward
            (concat
             "entity" ;; instance name
             "\s+"
             "\\([A-z,0-9]+\\)" ;; library
             "\s+"
             "is"
             ) nil t 1))
    (let ((match (match-string 1)))
      ;; if there is a match, intern it, otherwise return nil
      (if match (intern match) nil))))

;; FIXME: most of this can be shared between VHDL and verilog
;; convert to a macro..?
(defun hdldep--vhdl-get-entities (file)
  "Get all entities in VHDL file FILE."
  (require 'cl)
  (let ((entities nil))
    (with-temp-buffer

      (cl-flet
          ((extract-entity-name
            (regexp index)
            (progn
              (goto-char (point-min))
              (while (re-search-forward regexp nil t)
                (let ((match (match-string index)))
                  (when match
                    (push (intern match) entities)))))))

        (insert-file-contents file)
        (hdldep--vhdl-flatten-buffer)

        (extract-entity-name
         (concat "\\(" "[A-z,0-9]+" "\\)" ;; instance name
                 "\s*" ":" "\s*" "entity" "\s+" ;;
                 "\\(" "[A-z,0-9]+" "\\)" "\." ;; library
                 "\\(" "[A-z,0-9]+" "\\)" ;; entity
                 ) 3)


        (extract-entity-name
         (concat "\\([A-z,0-9]+\\)" ;; instance name
                 "\s*:\s*" ;;
                 "\\([A-z,0-9]+\\)"  ;; entity
                 "\s+\\(generic\\|port\\)\s+map"
                 ) 2)

        (goto-char (point-min))

        ))
    (delete-dups entities)))

;; fini

(provide 'hdl_deps)
;;;

;; (while (re-search-forward
;;         (concat
;;          "\\(" "[A-z,0-9]+" "\\)" ;; instance name
;;          "\s*" ":" "\s*" "entity" "\s+" ;;
;;          "\\(" "[A-z,0-9]+" "\\)" "\." ;; library
;;          "\\(" "[A-z,0-9]+" "\\)" ;; entity
;;          ) nil t)
;;   (let ((match (match-string 3)))
;;     (when match
;;       (push (intern match) entities)
;;       )))

;; find
;; (while (re-search-forward
;;         (concat
;;          "\\([A-z,0-9]+\\)" ;; instance name
;;          "\s*" ":" "\s*" ;;
;;          "\\([A-z,0-9]+\\)"  ;; entity
;;          "\s+"
;;          "\\(generic\\|port\\)"
;;          "\s+"
;;          "map"
;;          ) nil t)
;;   (let ((match (match-string 2)))
;;     (when match
;;       (push (intern match) entities)
;;       )))))
