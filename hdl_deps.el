;;; package -- hdl-deps
;;;
;;; Commentary:
;;;
;;; Code:
;;;
;;; TODO: vhdl packages
;;; TODO: ip cores
;;; TODO: auto synth mode

(require 'projectile)

;;------------------------------------------------------------------------------
;; Graphviz Generation
;;------------------------------------------------------------------------------

(defun hdldep-graph-current-buffer (dir)
  ""
  (interactive
   (list (read-directory-name "Search Directory:"
                              (projectile-project-root))))
  (print dir)
  (print (type-of dir))

  (hdldep--graph-buffer (buffer-file-name) dir))

(defun hdldep--graph-buffer (file searchdir)
  ""
  (let* ((file-base (file-name-base file))
         (entity (hdldep--vhdl-parse-entity-name file)))
    (with-temp-file
        (format "%s.gv" file-base)
      (insert (hdldep--gv-for-module searchdir (symbol-name entity))))
    (shell-command (format "dot -Tsvg %s.gv -o %s.svg" file-base file-base))
    (with-selected-window (selected-window)
      (switch-to-buffer-other-window (find-file-noselect (format "%s.svg" file-base) t))
      (revert-buffer t t))))

(defun hdldep--gv-for-module (dir module)

  "Search the directory DIR for files and create a digraph for the
selected MODULE.

Dir and module are both specified as strings."

  (hdldep--digraph-to-gv
   (hdldep--create-digraph-for-module dir module)))

(defun hdldep--digraph-to-gv (edges)

  "Takes a list of edges and converts it into graphviz dot format.

The list of edges takes the form of, for example:

((partition . deghost)
 (priority_encoder . r)
 (partition . priority_encoder)
 (pat_unit_mux . dav_to_phase)
 (pat_unit . priority_encoder)
 (pat_unit . hit_count)
 (pat_unit_mux . pat_unit)
 (partition . pat_unit_mux)
 (partition . dav_to_phase)
 (chamber . partition)
 (chamber_pulse_extension . pulse_extension)
 (chamber . chamber_pulse_extension)
 (chamber . dav_to_phase))

and returns the equivalent output as a graph{}"

  (let ((s ""))
    (setq s (concat s "graph {\n"))
    (dolist (edge edges)
      (setq s (concat s (format "   %s -- %s \n" (car edge) (cdr edge)))))
    (setq s (concat s "}\n"))))

;;------------------------------------------------------------------------------
;; Dependency Finding
;;------------------------------------------------------------------------------

(defun hdldep--create-digraph-for-module (dir module)

  "Search the directory DIR for files and create a digraph for the
selected MODULE.

Dir and module are both specified as strings."

  (delete-dups
   (hdldep--find-connected-edges
    (hdldep--get-all-edges (hdldep--get-all-hdl-files dir))
    (list)
    (intern module))))

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
  (split-string
   (shell-command-to-string
    (format (concat "find %s \\("
                    " -name \"*.vhdl\"" " -o"
                    " -name \"*.vhd\""  " -o"
                    " -name \"*.v\""    " -o"
                    " -name \"*.sv\" " "\\)") dir))))

(defun hdldep--get-all-edges (files)
  ""
  (let ((all-edges nil))
    (dolist (file files)
      (princ (format "Getting edges for %s...\n" file))
      (let ((edges (hdldep--deps-list-to-edges
                    (hdldep--make-deps-list file))))
        (if edges (setf all-edges (append edges all-edges)))))
    (delete-dups all-edges)))

(defun hdldep--deps-list-to-edges (deps-list)
  "convert a dependency list deps-list into dot lines.
car of the list is the module
cdr of the list is the dependencies"
  (let ((edges nil))
    (dolist (dep (cdr deps-list))
      (push (cons (car deps-list) dep) edges)) edges))

;;------------------------------------------------------------------------------
;; Language Agnostic Wrapper
;;------------------------------------------------------------------------------

(defun hdldep--file-is-verilog (file)
  ""
  (let ((ext (file-name-extension file)))
    (or (string= ext "v")
        (string= ext "sv"))))

(defun hdldep--file-is-vhdl (file)
  ""
  (let ((ext (file-name-extension file)))
    (or (string= ext "vhd")
        (string= ext "vhdl"))))

(defun hdldep--vhdl-or-verilog (file func-vhdl func-verilog)
  "Dispatches function calls to the right flavor of a function.
TODO: this is really ugly... "
  (cond ((hdldep--file-is-vhdl file) (funcall func-vhdl file))
        ((hdldep--file-is-verilog file) (funcall func-verilog file))
        (t (error (format  "Unrecognized file format %s" file)))))

(defun hdldep--make-deps-list (file)
  "For a given FILE, return a list that contains both the module
in the file itself along with all of its direct dependencies."
  (append
   (list (hdldep--parse-entity-name file))
   (hdldep--get-entities file)))

(defun hdldep--parse-entity-name (file)
  ""
  (hdldep--vhdl-or-verilog file
                           #'hdldep--verilog-parse-entity-name
                           #'hdldep--vhdl-parse-entity-name))

(defun hdldep--get-entities (file)
  ""
  (hdldep--vhdl-or-verilog file
                           #'hdldep--verilog-get-entities
                           #'hdldep--vhdl-get-entities))

;;------------------------------------------------------------------------------
;; Verilog Parsing
;;------------------------------------------------------------------------------

;; FIXME: make the vhdl and verilog common... put them in a macro..?

(defun hdldep--verilog-flatten-buffer ()

  "Flatten a Verilog buffer.

Removes all comments and newlines for easier processing as a stream."

  ;; remove all comments
  (while (re-search-forward "\/\/.*\n" nil t)
    (replace-match ""))
  (goto-char (point-min))

  ;; with comments removed, safe to remove all newlines
  (while (re-search-forward "\n" nil t)
    (replace-match " "))
  (goto-char (point-min)))

(defun hdldep--verilog-parse-entity-name (file)
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

  (require 'cl-lib)
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
            (push (intern match) entities)))))
    (delete-dups entities)))

;;------------------------------------------------------------------------------
;; VHDL Parsing
;;------------------------------------------------------------------------------

(defun hdldep--vhdl-flatten-buffer ()

  "Flatten a VHDL buffer.

Removes all comments and newlines for easier processing as a
stream."

  ;; remove all comments
  (while (re-search-forward "--.*\n" nil t)
    (replace-match ""))
  (goto-char (point-min))

  ;; with comments removed, safe to remove all newlines
  (while (re-search-forward "\n" nil t)
    (replace-match " "))
  (goto-char (point-min)))

(defun hdldep--vhdl-parse-entity-name (file)

  "Get the first entity name of a given VHDL file.

e.g. for a file which defines a module 'adder', this will return the
symbol 'adder"

  (with-temp-buffer
    (insert-file-contents file)
    (hdldep--vhdl-flatten-buffer)
    (while (re-search-forward
            (concat
             "entity" ;; instance name
             "\s+"
             "\\([A-z,0-9]+\\)" ;; library
             "\s+"
             "is") nil t 1))
    (let ((match (match-string 1)))
      ;; if there is a match, intern it, otherwise return nil
      (if match (intern match) nil))))

;; TODO: most of this can be shared between VHDL and verilog
;; convert to a macro..?
(defun hdldep--vhdl-get-entities (file)

  "Get all entities instantiated in a vhdl FILE.

Returns a list such as '(adder shift_register) "

  (require 'cl-lib)
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
         (concat "\\(" "[A-z,0-9]+" "\\)"       ;; instance name
                 "\s*" ":" "\s*" "entity" "\s+" ;;
                 "\\(" "[A-z,0-9]+" "\\)" "\."  ;; library
                 "\\(" "[A-z,0-9]+" "\\)"       ;; entity
                 ) 3)


        (extract-entity-name
         (concat "\\([A-z,0-9]+\\)" ;; instance name
                 "\s*:\s*"          ;;
                 "\\([A-z,0-9]+\\)" ;; entity
                 "\s+\\(generic\\|port\\)\s+map"
                 ) 2)

        (goto-char (point-min))))

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

(defvar teros-hdl-documenter-path
  "~/work/colibri/bin/teroshdl-hdl-documenter")

(defun teros-hdl-sm-diagram-from-buffer ()
  (interactive)
  (teros-hdl-sm-diagram-from-file (buffer-file-name)))

(defun teros-hdl-sm-diagram-from-file (file)
  (let ((tmp (format "/tmp/%s" (substring (md5 file) 0 8)))
        (entity-name (hdldep--parse-entity-name file)))

    (make-directory tmp t)

    (message (shell-command-to-string
              (format "%s -o markdown --fsm -i %s --outpath %s"
                      teros-hdl-documenter-path file tmp)))

    (let ((fname (format "%s/stm_%s_00.svg" tmp entity-name)))
      (when (file-exists-p fname)
        (with-selected-window (selected-window)
          (switch-to-buffer-other-window (find-file-noselect fname t))
          (revert-buffer t t))))))

(after! vhdl-mode
  (define-key vhdl-mode-map  "\C-c\C-p" 'teros-hdl-sm-diagram-from-buffer))

;;(teros-hdl-sm-diagram-from-file "/home/andrew/work/l0mdt-hdl-design/HAL/tdc/src/tdc_decoder/tdc_packet_processor.vhd")
;;(teros-hdl-sm-diagram-from-file "/home/andrew/work/drs-firmware-daq-debug/drs/src/drs.v")

(provide 'hdl-deps)
