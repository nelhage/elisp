;;; kotlin-ts-mode.el --- A mode for editing Kotlin files based on tree-sitter  -*- lexical-binding: t; -*-

;; Copyright 2022 Alex Figl-Brick

;; Author: Alex Figl-Brick <alex@alexbrick.me>
;; Package-Version: 20260512.1409
;; Package-Revision: 39e30e4cf803
;; Package-Requires: ((emacs "30.1"))
;; URL: https://gitlab.com/bricka/emacs-kotlin-ts-mode

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see
;; <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package uses the `treesit' functionality added in Emacs 29 to
;; provide a nice mode for editing Kotlin code.

;;; Code:

(require 'treesit)
(require 'project)
(require 'c-ts-common) ; For comment indent and filling
(eval-when-compile
  (require 'subr-x))

(declare-function treesit-parser-create "treesit.c")
(declare-function treesit-induce-sparse-tree "treesit.c")
(declare-function treesit-defun-name "treesit.c")
(declare-function treesit-node-next-sibling "treesit.c")
(declare-function treesit-node-end "treesit.c")
(declare-function treesit-node-start "treesit.c")
(declare-function treesit-node-child "treesit.c")
(declare-function treesit-node-type "treesit.c")
(declare-function treesit-search-subtree "treesit.c")
(declare-function treesit-thing-at-point "treesit.c")

(defcustom kotlin-ts-mode-indent-offset 4
  "Number of spaces for each indentation step in `kotlin-ts-mode'."
  :type 'natnum
  :safe #'natnump
  :group 'kotlin)

(defvar kotlin-ts-mode-syntax-table
  (let ((st (make-syntax-table)))
    ;; Adapted from java-ts-mode
    ;; https://git.savannah.gnu.org/cgit/emacs.git/tree/lisp/progmodes/java-ts-mode.el#n49
    ;; at 47d87112ffb7f0e2ff52d4abef6e259c00d5385c
    (modify-syntax-entry  ?_     "_"       st)
    (modify-syntax-entry  ?\\    "\\"      st)
    (modify-syntax-entry  ?+     "."       st)
    (modify-syntax-entry  ?-     "."       st)
    (modify-syntax-entry  ?=     "."       st)
    (modify-syntax-entry  ?%     "."       st)
    (modify-syntax-entry  ?<     "."       st)
    (modify-syntax-entry  ?>     "."       st)
    (modify-syntax-entry  ?&     "."       st)
    (modify-syntax-entry  ?|     "."       st)
    (modify-syntax-entry  ?\'    "\""      st)
    (modify-syntax-entry  ?\240  "."       st)
    (modify-syntax-entry  ?/     ". 124b"  st)
    (modify-syntax-entry  ?*     ". 23"    st)
    (modify-syntax-entry  ?\n    "> b"     st)
    (modify-syntax-entry  ?\r    "> b"     st)
    (modify-syntax-entry  ?\^m   "> b"     st)
    (modify-syntax-entry  ?@     "'"       st)
    (modify-syntax-entry  ?`     "\""      st)
    st))

(defconst kotlin-ts-mode--string-node-child-no-fontify
  '("interpolated_expression" "interpolated_identifier" "interpolation_identifier_start" "interpolation_expression_start" "interpolation_expression_end")
  "Node types in a string that should not be fontified by the string face.")

(defun kotlin-ts-mode--fontify-string (node override start end &rest _)
  "Fontify a string but not any substitutions inside of it.

See `treesit-font-lock-rules' for more details.  NODE is the string node.  START
and END mark the region to be fontified.  OVERRIDE is the override flag.

This function is heavily inspired by `js--fontify-template-string'."
  (if (treesit-node-child node 0)
      (let ((font-begin (treesit-node-start node))
            (font-end nil)
            (child (treesit-node-child node 0)))
        (while child
          (setq font-end (if (member (treesit-node-type child) kotlin-ts-mode--string-node-child-no-fontify)
                             (treesit-node-start child)
                           (treesit-node-end child)))
          (treesit-fontify-with-override font-begin font-end 'font-lock-string-face override start end)
          (setq font-begin (treesit-node-end child)
                child (treesit-node-next-sibling child)))
        (when (< font-begin end)
          ;; There are no more children, but we haven't reached the end of the string
          (treesit-fontify-with-override font-begin (treesit-node-end node) 'font-lock-string-face override start end)))

    ;; If we get here, then the string has no children: it's just a normal string
    (treesit-fontify-with-override (treesit-node-start node) (treesit-node-end node) 'font-lock-string-face override)))

;; Based on https://github.com/fwcd/tree-sitter-kotlin/pull/50
(defvar kotlin-ts-mode--treesit-settings
  (when (treesit-available-p)
    (treesit-font-lock-rules
     :language 'kotlin
     :feature 'keyword
     '(;; `it` keyword inside lambdas
       ;; FIXME: This will highlight the keyword outside of lambdas since tree-sitter
       ;;        does not allow us to check for arbitrary nestation
       ((simple_identifier) @font-lock-keyword-face (:equal @font-lock-keyword-face "it"))
       ((interpolated_identifier) @font-lock-keyword-face (:equal @font-lock-keyword-face "it"))

       ;; `field` keyword inside property getter/setter
       ;; FIXME: This will highlight the keyword outside of getters and setters
       ;;        since tree-sitter does not allow us to check for arbitrary nestation
       ((simple_identifier) @font-lock-keyword-face (:equal @font-lock-keyword-face "field"))

       ;; `super` keyword inside classes
       (super_expression) @font-lock-keyword-face

       ["val" "var" "enum" "class" "object" "interface"] @font-lock-keyword-face

       ["package" "import" "as" "as?"] @font-lock-keyword-face

       ["by"
        "constructor"
        "init"
        "this"
        "where"
        "suspend"
        ] @font-lock-keyword-face

       (type_alias "typealias" @font-lock-keyword-face)
       [(class_modifier)
        (member_modifier)
        (function_modifier)
        (property_modifier)
        (platform_modifier)
        (variance_modifier)
        (parameter_modifier)
        (visibility_modifier)
        (reification_modifier)
        (inheritance_modifier)
        ] @font-lock-keyword-face

       ["break"
        "break@"
        "catch"
        "companion"
        "continue"
        "continue@"
        "do"
        "else"
        "finally"
        "for"
        "fun"
        "if"
        "in"
        "is"
        "return"
        "return@"
        "throw"
        "try"
        "when"
        "while"
        ] @font-lock-keyword-face

       (infix_expression (simple_identifier) @font-lock-keyword-face (:equal @font-lock-keyword-face "to"))

       (prefix_expression "!" @font-lock-negation-char-face))

     :language 'kotlin
     :feature 'comment
     '(((multiline_comment) @font-lock-doc-face (:match "^/\\*\\*" @font-lock-doc-face))
       [(line_comment) (multiline_comment) (shebang_line)] @font-lock-comment-face)

     :language 'kotlin
     :feature 'string
     '((character_literal) @font-lock-string-face
       (string_literal) @kotlin-ts-mode--fontify-string
       (string_literal
        [
         (interpolation_identifier_start)
         (interpolation_expression_start)
         (interpolation_expression_end)
         ] @font-lock-builtin-face))

     :language 'kotlin
     :feature 'escape-sequence
     :override t
     '((character_escape_seq) @font-lock-escape-face)

     :language 'kotlin
     :feature 'definition
     '((function_declaration (simple_identifier) @font-lock-function-name-face)
       (parameter (simple_identifier) @font-lock-variable-name-face)
       (class_parameter (simple_identifier) @font-lock-variable-name-face)
       (variable_declaration (simple_identifier) @font-lock-variable-name-face))

     :language 'kotlin
     :feature 'number
     '([(integer_literal) (long_literal) (hex_literal) (bin_literal) (unsigned_literal) (real_literal)] @font-lock-number-face)

     :language 'kotlin
     :feature 'type
     '((type_identifier) @font-lock-type-face
       (enum_entry (simple_identifier) @font-lock-type-face)
       (call_expression (simple_identifier) @font-lock-type-face
                        (:match "^[A-Z]" @font-lock-type-face))
       (navigation_expression (simple_identifier) @font-lock-type-face
                              (:match "^[A-Z]" @font-lock-type-face)))

     :language 'kotlin
     :feature 'function
     '((call_expression (navigation_expression (navigation_suffix (simple_identifier) @font-lock-function-name-face)))
       (call_expression (simple_identifier) @font-lock-function-name-face)
       (infix_expression _ (simple_identifier) @font-lock-function-name-face _))

     :language 'kotlin
     :feature 'property
     '((navigation_expression (navigation_suffix (simple_identifier) @font-lock-property-face)))

     :language 'kotlin
     :feature 'constant
     :override t
     '([(null_literal) (boolean_literal)] @font-lock-constant-face
       ((simple_identifier) @font-lock-constant-face
        (:match "^[A-Z_][A-Z_0-9]*$" @font-lock-constant-face)))

     :language 'kotlin
     :feature 'builtin
     :override t
     '((call_expression (simple_identifier) @font-lock-builtin-face
                        (:equal @font-lock-builtin-face "listOf"))
       (call_expression (simple_identifier) @font-lock-builtin-face
                        (:equal @font-lock-builtin-face "arrayOf"))
       (call_expression (simple_identifier) @font-lock-builtin-face
                        (:equal @font-lock-builtin-face "arrayOfNulls"))
       (call_expression (simple_identifier) @font-lock-builtin-face
                        (:equal @font-lock-builtin-face "byteArrayOf"))
       (call_expression (simple_identifier) @font-lock-builtin-face
                        (:equal @font-lock-builtin-face "shortArrayOf"))
       (call_expression (simple_identifier) @font-lock-builtin-face
                        (:equal @font-lock-builtin-face "intArrayOf"))
       (call_expression (simple_identifier) @font-lock-builtin-face
                        (:equal @font-lock-builtin-face "longArrayOf"))
       (call_expression (simple_identifier) @font-lock-builtin-face
                        (:equal @font-lock-builtin-face "ubyteArrayOf"))
       (call_expression (simple_identifier) @font-lock-builtin-face
                        (:equal @font-lock-builtin-face "ushortArrayOf"))
       (call_expression (simple_identifier) @font-lock-builtin-face
                        (:equal @font-lock-builtin-face "uintArrayOf"))
       (call_expression (simple_identifier) @font-lock-builtin-face
                        (:equal @font-lock-builtin-face "ulongArrayOf"))
       (call_expression (simple_identifier) @font-lock-builtin-face
                        (:equal @font-lock-builtin-face "floatArrayOf"))
       (call_expression (simple_identifier) @font-lock-builtin-face
                        (:equal @font-lock-builtin-face "doubleArrayOf"))
       (call_expression (simple_identifier) @font-lock-builtin-face
                        (:equal @font-lock-builtin-face "booleanArrayOf"))
       (call_expression (simple_identifier) @font-lock-builtin-face
                        (:equal @font-lock-builtin-face "charArrayOf"))
       (call_expression (simple_identifier) @font-lock-builtin-face
                        (:equal @font-lock-builtin-face "emptyArray"))
       (call_expression (simple_identifier) @font-lock-builtin-face
                        (:equal @font-lock-builtin-face "mapOf"))
       (call_expression (simple_identifier) @font-lock-builtin-face
                        (:equal @font-lock-builtin-face "setOf"))
       (call_expression (simple_identifier) @font-lock-builtin-face
                        (:equal @font-lock-builtin-face "listOf"))
       (call_expression (simple_identifier) @font-lock-builtin-face
                        (:equal @font-lock-builtin-face "sequenceOf"))
       (call_expression (simple_identifier) @font-lock-builtin-face
                        (:equal @font-lock-builtin-face "emptyMap"))
       (call_expression (simple_identifier) @font-lock-builtin-face
                        (:equal @font-lock-builtin-face "emptySet"))
       (call_expression (simple_identifier) @font-lock-builtin-face
                        (:equal @font-lock-builtin-face "emptyList"))
       (call_expression (simple_identifier) @font-lock-builtin-face
                        (:equal @font-lock-builtin-face "emptySequence"))
       (call_expression (simple_identifier) @font-lock-builtin-face
                        (:equal @font-lock-builtin-face "mutableMapOf"))
       (call_expression (simple_identifier) @font-lock-builtin-face
                        (:equal @font-lock-builtin-face "mutableSetOf"))
       (call_expression (simple_identifier) @font-lock-builtin-face
                        (:equal @font-lock-builtin-face "mutableListOf"))
       (call_expression (simple_identifier) @font-lock-builtin-face
                        (:equal @font-lock-builtin-face "print"))
       (call_expression (simple_identifier) @font-lock-builtin-face
                        (:equal @font-lock-builtin-face "println"))
       (call_expression (simple_identifier) @font-lock-builtin-face
                        (:equal @font-lock-builtin-face "error"))
       (call_expression (simple_identifier) @font-lock-builtin-face
                        (:equal @font-lock-builtin-face "TODO"))
       (call_expression (simple_identifier) @font-lock-builtin-face
                        (:equal @font-lock-builtin-face "run"))
       (call_expression (simple_identifier) @font-lock-builtin-face
                        (:equal @font-lock-builtin-face "runCatching"))
       (call_expression (simple_identifier) @font-lock-builtin-face
                        (:equal @font-lock-builtin-face "repeat"))
       (call_expression (simple_identifier) @font-lock-builtin-face
                        (:equal @font-lock-builtin-face "lazy"))
       (call_expression (simple_identifier) @font-lock-builtin-face
                        (:equal @font-lock-builtin-face "lazyOf"))
       (call_expression (simple_identifier) @font-lock-builtin-face
                        (:equal @font-lock-builtin-face "enumValues"))
       (call_expression (simple_identifier) @font-lock-builtin-face
                        (:equal @font-lock-builtin-face "enumValueOf"))
       (call_expression (simple_identifier) @font-lock-builtin-face
                        (:equal @font-lock-builtin-face "assert"))
       (call_expression (simple_identifier) @font-lock-builtin-face
                        (:equal @font-lock-builtin-face "check"))
       (call_expression (simple_identifier) @font-lock-builtin-face
                        (:equal @font-lock-builtin-face "checkNotNull"))
       (call_expression (simple_identifier) @font-lock-builtin-face
                        (:equal @font-lock-builtin-face "require"))
       (call_expression (simple_identifier) @font-lock-builtin-face
                        (:equal @font-lock-builtin-face "requireNotNull"))
       (call_expression (simple_identifier) @font-lock-builtin-face
                        (:equal @font-lock-builtin-face "with"))
       (call_expression (simple_identifier) @font-lock-builtin-face
                        (:equal @font-lock-builtin-face "suspend"))
       (call_expression (simple_identifier) @font-lock-builtin-face
                        (:equal @font-lock-builtin-face "synchronized")))

     :language 'kotlin
     :feature 'variable
     '((import_header (identifier (simple_identifier) @font-lock-constant-face
                                  (:match "^[A-Z_][A-Z_0-9]*$" @font-lock-constant-face)))
       (import_header (identifier (simple_identifier) @font-lock-type-face
                                  (:match "^[A-Z]" @font-lock-type-face)))
       (simple_identifier) @font-lock-variable-name-face
       (interpolated_identifier) @font-lock-variable-name-face))))

(defconst kotlin-ts-mode--treesit-indent-rules
  '((kotlin
     ((node-is "}") parent-bol 0)
     ((node-is ")") parent-bol 0)
     ((parent-is "anonymous_initializer") parent-bol kotlin-ts-mode-indent-offset)
     ((parent-is "statements") parent-bol 0)
     ((parent-is "catch_block") parent-bol kotlin-ts-mode-indent-offset)
     ((parent-is "class_body") parent-bol kotlin-ts-mode-indent-offset)
     ((parent-is "control_structure_body") parent-bol kotlin-ts-mode-indent-offset)
     ((parent-is "finally_block") parent-bol kotlin-ts-mode-indent-offset)
     ((parent-is "function_body") parent-bol kotlin-ts-mode-indent-offset)
     ((parent-is "function_value_parameters") parent-bol kotlin-ts-mode-indent-offset)
     ((parent-is "lambda_literal") parent-bol kotlin-ts-mode-indent-offset)
     ((parent-is "primary_constructor") parent-bol kotlin-ts-mode-indent-offset)
     ((parent-is "secondary_constructor") parent-bol kotlin-ts-mode-indent-offset)
     ((parent-is "try_expression") parent-bol kotlin-ts-mode-indent-offset)
     ((parent-is "value_arguments") parent-bol kotlin-ts-mode-indent-offset)
     ((parent-is "when_expression") parent-bol kotlin-ts-mode-indent-offset)
     ((parent-is "comment") parent-bol 1)
     ((node-is "navigation_suffix") parent-bol kotlin-ts-mode-indent-offset)
     (catch-all prev-sibling 0))))

;; Imenu

(defun kotlin-ts-mode--defun-name (node)
  "Return the name of the defun node if NODE is a defun node.

Else return nil."
  (pcase (treesit-node-type node)
    ("class_declaration"
     (treesit-node-text (treesit-search-subtree node (regexp-quote "type_identifier") nil nil 1) t))
    ("function_declaration"
     (treesit-node-text (treesit-search-subtree node (regexp-quote "simple_identifier") nil nil 1) t))))

(defun kotlin-ts-mode--imenu-1 (tree)
  "Helper for `kotlin-ts-mode--imenu'.

Take in a sparse tree TREE and map the symbols to their positions."
  (mapcar (lambda (child) (cons (treesit-defun-name child) (treesit-node-start child))) (flatten-list tree)))

(defun kotlin-ts-mode--imenu ()
  "Return Imenu alist for the current buffer."
  (let* ((root-node (treesit-buffer-root-node))
         (class-tree (treesit-induce-sparse-tree root-node "^class_declaration$" nil 10))
         (class-entries (kotlin-ts-mode--imenu-1 class-tree))
         (function-tree (treesit-induce-sparse-tree root-node "^function_declaration$" nil 10))
         (function-entries (kotlin-ts-mode--imenu-1 function-tree)))
    (append
     (when class-entries `(("Class" . ,class-entries)))
     (when function-entries `(("Function" . ,function-entries))))))

(defun kotlin-ts-mode-goto-test-file ()
  "Go from the current file to the test file."
  (interactive)
  (if (not (string-match-p (regexp-quote "src/main/kotlin") (buffer-file-name)))
      (warn "Could not find test file for %s" (buffer-file-name))
    (let* ((test-directory (file-name-directory (string-replace "src/main/kotlin" "src/test/kotlin" (buffer-file-name))))
           (file-name-as-test (concat (file-name-base (buffer-file-name)) "Test.kt"))
           (test-file-location (concat test-directory file-name-as-test)))
      (find-file test-file-location))))

(defun kotlin-ts-mode--get-package-name ()
  "Determine the name of the package of the current file."
  (let* ((root-node (treesit-buffer-root-node))
         (package-node (treesit-search-subtree root-node (regexp-quote "package_header"))))
    (when package-node
      (treesit-node-text (treesit-node-child package-node 1) t))))

(defun kotlin-ts-mode--get-class-name ()
  "Determine the name of the class containing point."
  (let ((class-names nil)
        (class-node (treesit-thing-at-point (regexp-quote "class_declaration") 'nested)))
    (while class-node
      (push (treesit-defun-name class-node) class-names)
      (setq class-node
            (treesit-parent-until
             class-node
             (lambda (node) (equal (treesit-node-type node) "class_declaration")))))
    (when class-names
      (string-join class-names "$"))))

(defun kotlin-ts-mode--get-function-name ()
  "Determine the name of the function containing point."
  (let ((function-node (treesit-thing-at-point (regexp-quote "function_declaration") 'nested)))
    (when function-node (treesit-defun-name function-node))))

(defun kotlin-ts-mode--qualify-name (&rest names)
  "Return a string that fully qualifies the given NAMES.

This function will strip out any surrounding backtick characters
in the individual names."
  (string-join (mapcar
                (lambda (name)
                  (replace-regexp-in-string "^`" "" (replace-regexp-in-string "`$" "" name)))
                names)
               "."))

(defun kotlin-ts-mode--in-gradle-project-p ()
  "Return t if the current buffer is in a project with a local Gradle installation."
  (file-exists-p (string-join `(,(project-root (project-current)) "gradlew") "/")))

(defun kotlin-ts-mode--compilation-buffer-name-function (_mode)
  "The name of the buffer used for Gradle commands."
  "*kotlin-ts-mode[gradle]*")

(defun kotlin-ts-mode--run-gradle-command (project task args)
  "Run the given Gradle TASK with the given ARGS in the given PROJECT.

If PROJECT is nil, run in root project."
  (let ((default-directory default-directory)
        (exec-path exec-path)
        (command "gradle")
        (compilation-buffer-name-function #'kotlin-ts-mode--compilation-buffer-name-function)
        (qualified-task (if project (concat ":" project ":" task) (concat ":" task))))
    (when (kotlin-ts-mode--in-gradle-project-p)
      (setq default-directory (project-root (project-current))
            command "./gradlew"
            exec-path (list nil)))
    (compile
     (concat
      command
      " --console=plain"
      " "
      qualified-task
      " "
      (string-join
       (mapcar #'shell-quote-argument args)
       " ")))))

(defun kotlin-ts-mode--path-chunk-to-gradle-project (chunk)
  "Convert given CHUNK to a Gradle project name.

For example:
foo --> foo
foo/bar --> foo:bar"
  (string-replace "/" ":" (string-remove-suffix "/" chunk)))

(defun kotlin-ts-mode--get-subproject-name ()
  "Determine the name of the subproject of the current buffer.
Return nil if root project.

We use a simple heuristic here: we compare the location of the
closest `build.gradle.kts' file with the root of the project and
treat that difference as the subproject name. We do not support
custom project names."
  (let ((root-directory (project-root (project-current)))
        (gradle-dir (locate-dominating-file (buffer-file-name) "build.gradle.kts")))
    (cond
     ((not gradle-dir) nil)
     ((string-equal gradle-dir root-directory) nil)
     (t (kotlin-ts-mode--path-chunk-to-gradle-project (string-remove-prefix root-directory gradle-dir))))))

(defun kotlin-ts-mode-run-current-test-class ()
  "Run the current test class."
  (interactive)
  (let* ((package-name (kotlin-ts-mode--get-package-name))
         (class-name (kotlin-ts-mode--get-class-name)))
    (if (not (and package-name class-name))
        (warn "Could not find the package and class name.")
      (kotlin-ts-mode--run-gradle-command
       (kotlin-ts-mode--get-subproject-name)
       "test"
       (list
        "--tests"
         (kotlin-ts-mode--qualify-name
          package-name
          class-name))))))

(defun kotlin-ts-mode-run-current-test-function ()
  "Run the current test function."
  (interactive)
  (let* ((package-name (kotlin-ts-mode--get-package-name))
         (class-name (kotlin-ts-mode--get-class-name))
         (function-name (kotlin-ts-mode--get-function-name)))
    (if (not (and package-name class-name function-name))
        (warn "Could not find the package, class, and function name.")
      (kotlin-ts-mode--run-gradle-command
       (kotlin-ts-mode--get-subproject-name)
       "test"
       (list
        "--tests"
         (kotlin-ts-mode--qualify-name
          package-name
          class-name
          function-name))))))

;; Find Files
(defvar kotlin-ts-mode--find-sibling-rules
  ;; '(("\\\\([^/]+\\\\)\\\\.kt\\"))
  (list
   (list (rx "main/" (group (+ not-newline) "/") (group (+ (not "/"))) ".kt") "test/\\1\\2Test.kt")
   (list (rx "test/" (group (+ not-newline) "/") (group (+ (not "/"))) "Test.kt") "main/\\1\\2.kt"))
  "Rules for finding sibling files as defined by `find-sibling-rules'.

Current rules are:
1. From implementation to corresponding test
2. From test to corresponding implementation")

;;;###autoload
(define-derived-mode kotlin-ts-mode prog-mode "Kotlin"
  "Major mode for editing Kotlin using tree-sitter."
  :syntax-table kotlin-ts-mode-syntax-table

  (when (treesit-ready-p 'kotlin)
    (treesit-parser-create 'kotlin)

    (setq-local treesit-defun-name-function #'kotlin-ts-mode--defun-name)

    ;; Comments
    (c-ts-common-comment-setup)

    ;; Electric
    (setq-local electric-indent-chars
                (append "{}():;," electric-indent-chars))

    ;; Syntax Highlighting
    (setq-local treesit-font-lock-settings kotlin-ts-mode--treesit-settings)
    (setq-local treesit-font-lock-feature-list '((comment number string definition)
                                                 (keyword builtin type constant variable)
                                                 (escape-sequence function property)))

    ;; Indent
    (setq-local editorconfig-indent-size-vars '(kotlin-ts-mode-indent-offset))
    (setq-local treesit-simple-indent-rules kotlin-ts-mode--treesit-indent-rules)

    ;; Imenu
    (setq-local imenu-create-index-function #'kotlin-ts-mode--imenu)
    (setq-local which-func-functions nil)

    ;; Find Files
    (setq-local find-sibling-rules kotlin-ts-mode--find-sibling-rules)

    (treesit-major-mode-setup)))

(derived-mode-add-parents 'kotlin-ts-mode '(kotlin-mode))

(provide 'kotlin-ts-mode)
;;; kotlin-ts-mode.el ends here
