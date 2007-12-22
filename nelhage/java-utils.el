(require 'utils)

(defun java-package-name ()
  "Return the package of the current java file"
  (save-excursion
    (beginning-of-buffer)
    (if (re-search-forward "package \\(.*\\);" nil t)
        (match-string 1)
      "")))

(defun java-class-name ()
  "Returne the name of the class represented in the current java
file."
  (string-match "\\(.*\\)\\.java" (buffer-name))
  (match-string 1 (buffer-name)))

(defun java-sort-imports ()
  "Sort any import statements in the current java file"
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (re-search-forward "^import" nil t)
    (beginning-of-line)
    (let ((start (point)))
      (while (looking-at "import")
                (forward-line 1))
      (sort-lines nil start (point)))))

(defun create-empty-javadoc-var-header ()
  "Create a blank javadoc style comment for a class data member."
  (interactive)
  (beginning-of-line-text)
  (progn (insert "/**\n* \n*/\n"))
  (indent-according-to-mode)
  (previous-line 3) (indent-according-to-mode)
  (next-line 1) (indent-according-to-mode)
  (next-line 1) (indent-according-to-mode)
  (previous-line 1) (end-of-line))

(defun create-section-header (&optional arg)
  "Insert a java section header."
  (interactive "MHeader text: ")
  (when (null arg) (setq arg ""))
  (indenting-changes (beginning-of-line)
    (insert "    //"
            (make-string 76 ?-)
            "\n")
    (insert "    // " arg "\n")
    (insert "    //"
            (make-string 76 ?-)
            "\n\n")))

(defun create-main-method (&optional arg)
  "Insert a template java main() method at point. With numeric
prefix argument, generate prototype usage() function as well, and
code to expect that many arguments"
  (interactive "P")
  (undo-boundary)
  (let ((full-class (concat (java-package-name) "." (java-class-name))))
    (indenting-changes 
     (create-section-header "Main method")
     (if arg
         (insert "private static void usage (String msg) {\n"
                 "System.out.println(\"usage:\");\n" ;
                 "System.out.println(msg);\n" 
                 "System.out.println(\"java " full-class "\");\n"
                 "System.exit(1);\n}\n\n"))
     (insert "public static void main (String[] args) {\n try{\n")
     (if arg
         (insert "int expectedArgs = " (format "%d" arg) ";\n"
                 "if (args.length != expectedArgs) {\n"
                 "usage(expectedArgs+\" were expected, "
                 "you provided \"+args.length);\n"
                 "}\n"))
     (insert "} catch (Exception e) {\n"
             "e.printStackTrace();\n"
             "System.exit(1);\n}\n"
             "System.exit(0);\n}\n"))))

(provide 'java-utils)
