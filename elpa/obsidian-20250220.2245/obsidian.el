;;; obsidian.el --- Obsidian Notes interface -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright (c) 2022 Mykhaylo Bilyanskyy <mb@blaster.ai>

;; Author: Mykhaylo Bilyanskyy
;; URL: https://github.com/licht1stein/obsidian.el
;; Keywords: obsidian, pkm, convenience
;; Package-Version: 20250220.2245
;; Package-Revision: 0b31775d5da1
;; Package-Requires: ((emacs "27.2") (f "0.2.0") (s "1.12.0") (dash "2.13") (markdown-mode "2.5") (elgrep "1.0.0") (yaml "0.5.1") (ht "2.3"))
;; This file is NOT part of GNU Emacs.

;;; License:
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.


;;; Commentary:
;; Obsidian.el lets you interact with more convenience with markdown files
;; that are contained in an Obsidian Notes vault.  It adds autocompletion for
;; tags and links, jumping between notes, capturing new notes into inbox etc.
;;
;; This allows you to use Emacs for editing your notes, leaving the Obsidian
;; app for syncing and doing more specialized stuff, like viewing notes graphs.

;;; Code:

(require 'f)
(require 'dash)
(require 's)
(require 'ht)
(require 'cl-lib)

(require 'markdown-mode)
(require 'elgrep)
(require 'yaml)

(defgroup obsidian nil "Obsidian Notes group." :group 'text)

(defvar obsidian--relative-path-length nil
  "Length of path of `obisidan-directory' used to calculate file relative paths.")

(defcustom obsidian-directory ""
  "Path to Obsidian Notes vault."
  :group 'obsidian
  :type 'directory
  :initialize #'custom-initialize-reset
  :set (lambda (symbol value)
         (let ((full-path (expand-file-name value)))
           (if (file-exists-p full-path)
               (progn
                 (message "Setting %s to %s" symbol full-path)
                 (set-default symbol full-path)
                 (setq obsidian--relative-path-length
                       (length (file-name-as-directory full-path))))
             (user-error (format "Directory %s doesn't exist" full-path))))))

(defcustom obsidian-inbox-directory nil
  "Subdir to create notes using `obsidian-capture'."
  :type 'directory)

(defcustom obsidian-daily-notes-directory obsidian-inbox-directory
  "Subdir to create daily notes with `obsidian-daily-note'.

Default is the inbox directory"
  :type 'directory)

(defcustom obsidian-templates-directory nil
  "Subdirectory containing templates."
  :type 'directory)

(defcustom obsidian-daily-note-template nil
  "Daily notes' template filename in templates directory."
  :type 'file)

(defcustom obsidian-include-hidden-files t
  "If true, files beginning with a period are considered valid Obsidian files."
  :type 'boolean)

(defcustom obsidian-excluded-directories nil
  "List of directories to exclude from Obsidian file searches.
Each directory should be a full path relative to `obsidian-directory`."
  :type '(repeat directory))

(defcustom obsidian-create-unfound-files-in-inbox t
  "Controls where to create a file when target file is missing.

Controls where to create a new file when visiting a link when the target is
missing.  If true, create in inbox, otherwise create it in the same
directory as the current buffer."
  :type 'boolean)

(defcustom obsidian-links-use-vault-path nil
  "If true, use the full vault path for a link instead of just the filename."
  :type 'boolean)

(defcustom obsidian-wiki-link-alias-first nil
  "When non-nil, treat aliased wiki links like [[alias text|PageName]].
Otherwise, they will be treated as [[PageName|alias text]].
Maps to `markdown-wiki-link-alias-first'.  Included here because the default
for `obsidian.el' is different than that of `markdown-mode'."
  :type 'boolean
  :initialize #'custom-initialize-reset
  :set (lambda (symbol value)
         (set-default symbol value)
         (customize-set-value 'markdown-wiki-link-alias-first value)))

(defcustom obsidian-wiki-link-space-sub-char " "
  "Character to use instead of spaces when mapping wiki links to filenames.
Maps to `markdown-link-space-sub-char'.  Included here because the default
for `obsidian.el' is different than that of `markdown-mode'."
  :type 'char
  :initialize #'custom-initialize-reset
  :set (lambda (symbol value)
         (set-default symbol value)
         (customize-set-value 'markdown-link-space-sub-char value)))

(defcustom obsidian-debug-messages nil
  "If enabled, additional messages will be displayed for debugging."
  :type 'boolean)

(eval-when-compile (defvar local-minor-modes))

(defun obsidian--directory-files-pre28
    (orig-func dir &optional full match nosort _)
  "Version of `directory-files' compatible with Emacs versions < 28.

ORIG-FUNC is the original `directory-files' function that is going to be
advised,and DIR and the directory of files on which `directory-files' will
be called.
FULL, MATCH, and NOSORT are the optional arguments for the `directory-files'
function, while _ is the optional 4th argument used with newer versions
of `dirctory-files'."
  (apply orig-func dir full match nosort))

(if (< emacs-major-version 28)
    (advice-add 'directory-files :around #'obsidian--directory-files-pre28))

;;;###autoload
(defun obsidian-change-vault (&optional path)
  "Set vault directory to PATH and repopulate vault cache.
When run interactively asks user to specify the path."
  (interactive)
  (let* ((raw-path (or (and path (expand-file-name path))
                       (read-directory-name "Specify path to Obsidian vault: ")))
         (final-path (expand-file-name raw-path)))
    (if (file-exists-p final-path)
        (progn
          (customize-set-value 'obsidian-directory final-path)
          (message "Obsidian vault set to: %s" obsidian-directory)
          (obsidian-rescan-cache))
      (user-error (format "Directory %s doesn't exist" final-path)))))

(define-minor-mode obsidian-mode
  "Toggle minor `obsidian-mode' on and off.

Interactively with no argument, this command toggles the mode.
A positive prefix argument enables the mode, any other prefix
argument disables it.  From Lisp, argument omitted or nil enables
the mode, `toggle' toggles the state."
  ;; The initial value.
  :init-value nil
  :lighter " obs"
  :after-hook (obsidian-update)
  :keymap (make-sparse-keymap))

(defun obsidian--message (msg &optional file)
  "Send MSG to the message buffer specifying the optional FILE and return nil."
  (if file
      (message "%s for file %s" msg file)
    (message "%s" msg))
  nil)

(defconst obsidian--tag-regex
  "\\(?:\\`\\|[[:space:]]\\)\\#\\(?:[A-Za-z_/][-A-Za-z0-9_/]*[A-Za-z_/][-A-Za-z0-9_/]*\\)"
  "Regex pattern used to find tags in Obsidian files.

Here's a breakdown of the pattern:

- `\\(?:\\`\\|[[:space:]]\\)`: Matches the beginning of the document (`\\``) or
any whitespace character (`[[:space:]]`). This ensures that the tag either
starts at the beginning of the document or is preceded by a whitespace character.

- `\\(#`: Matches the starting hashtag of the tag and begins a capturing group
for the tag itself.

- `\\(?:[A-Za-z_/][-A-Za-z0-9_/]*[A-Za-z_/][-A-Za-z0-9_/]*\\)`: This ensures that
the tag has at least one non-numerical character (A-Z) and allows the use of
letters,numbers, underscores, hyphens, and forward slashes. The first and last
segments (`[A-Za-z_/]`) ensure that the tag contains at least one non-numeric
character outside the start and end. Numbers will not be allowed as the only
characters of a tag.

- `[#)]`: Ends the capturing group for the whole tag that starts with a `#`.")

(defconst obsidian-wiki-link-regex "\\[\\[[[:graph:][:blank:]]*\\]\\]"
  "Regex pattern used to find wikilinks.")

(defconst obsidian-markdown-link-regex "\\[[[:graph:][:blank:]]+\\]\([[:graph:][:blank:]]*\)"
  "Regex pattern used to find markdown links.")

(defvar obsidian-vault-cache nil
  "Cache for Obsidian files.

The cache is a hashmap with the following structure
{<filepath>: {tags: <list-of-tags>
              aliases: <list-of-aliases>}}
              links: <list-of-link-lists>}}

Each link list contains the following as returned by markdown-link-at-pos:
  0. beginning position
  1. end position
  2. link text
  3. URL
  4. reference label
  5. title text
  6. bang (nil or \"!\")")

(defvar obsidian--tags-map nil "Hash table with tags as keys and list of files as values.")

(defvar obsidian--file-metadata nil "Hash table with file metadata (tags, aliases, links.")

(defvar obsidian--aliases-map (make-hash-table :test 'equal) "Hash table of all Obsidian aliases.")

(defvar obsidian--backlinks-alist (make-hash-table :test 'equal) "Alist of backlinks.")

(defvar obsidian--jump-list nil "List of buffer locations visited via jump.")

(defvar obsidian--update-timer nil "Timer to periodically update the cache.")

(defvar obsidian--updated-time 0.0
  "Time of last cache update as a float number of seconds since the epoch.")

(defun obsidian--set-tags (file tag-list)
  "Set list TAG-LIST to FILE in files cache."
  (when tag-list
    (if-let ((attr-map (gethash file obsidian-vault-cache)))
        (puthash 'tags tag-list attr-map)
      (message "Unable to add tags for %s:\nAvailable keys:\n%s"
               file (s-join "\n" (hash-table-keys obsidian-vault-cache))))))

(defun obsidian--set-aliases (file alias-list)
  "Set list ALIAS-LIST to FILE in files cache."
  ;; Update alias hashtable
  (when alias-list
    (if-let ((maliases (obsidian--mapped-aliases file)))
        (let ((new-aliases (-difference alias-list maliases))
              (stale-aliases (-difference maliases alias-list)))
          (when new-aliases
            (seq-map (lambda (alias) (obsidian--add-alias alias file)) new-aliases))
          (when stale-aliases
            (seq-map (lambda (alias) (obsidian--remove-alias alias)) stale-aliases)))
      (seq-map (lambda (alias) (obsidian--add-alias alias file)) alias-list))
    (when-let ((attr-map (gethash file obsidian-vault-cache)))
      (puthash 'aliases alias-list attr-map))))

(defun obsidian--set-links (file links-map)
  "Set table LINKS-MAP to FILE in files cache."
  (when links-map
    (if-let ((attr-map (gethash file obsidian-vault-cache)))
        (puthash 'links links-map attr-map)
      (message "Unable to add links for %s:\nAvailable keys:\n%s"
               file (s-join "\n" (hash-table-keys obsidian-vault-cache))))))

(defun obsidian--add-alias (alias file)
  "Add ALIAS as key to `obsidian--aliases-map' with FILE as value."
  (puthash alias file obsidian--aliases-map))

(defun obsidian--remove-alias (alias)
  "Remove ALIAS as key to `obsidian--aliases-map'."
  (remhash alias obsidian--aliases-map))

(defun obsidian--get-alias (alias &optional default)
  "Find ALIAS in `obsidian--aliases-map' with optional DEFAULT."
  (gethash alias obsidian--aliases-map default))

(defun obsidian-aliases ()
  "Return all existing aliases (without values)."
  (hash-table-keys obsidian--aliases-map))

(defun obsidian-user-directory-p (&optional file)
  "Return t if FILE is a user defined directory inside `obsidian-directory'."
  (and (file-directory-p file)
       (obsidian-not-dot-obsidian-p file)
       (obsidian-not-trash-p file)
       (obsidian-not-in-excluded-directory-p file)))

(defun obsidian-dot-file-p (p)
  "Return t if path P points to a dot file."
  (s-starts-with-p "." (file-name-base p)))

(defun obsidian-not-trash-p (file)
  "Return t if FILE is not in .trash of Obsidian."
  (not (s-contains-p "/.trash" file)))

(defun obsidian-not-dot-obsidian-p (file)
  "Return t if FILE is not in .obsidian dir of Obsidian."
  (not (s-contains-p "/.obsidian" file)))

(defun obsidian-not-in-excluded-directory-p (file)
  "Check if FILE is not in any of the excluded directories."
  (not
   (seq-some
    (lambda (excluded-dir)
      (s-starts-with-p (expand-file-name excluded-dir) file))
    obsidian-excluded-directories)))

(defun obsidian-file-p (&optional file)
  "Return t if FILE is an obsidian.el file, nil otherwise.

If FILE is not specified, use the current buffer's file-path.
FILE is an Org-roam file if:
- It's located somewhere under `obsidian-directory
- It is a markdown .md file
- Is not a dot file or, if `obsidian-include-hidden-files' is t, then:
  - It is not in .trash
  - It is not an Emacs temp file"
  (-when-let* ((raw-path (or file (buffer-file-name (buffer-base-buffer))))
               (path (expand-file-name raw-path))
               (in-vault (s-starts-with-p obsidian-directory path))
               (md-ext (s-ends-with-p ".md" path))
               (not-dot-file (or obsidian-include-hidden-files
                                 (not (obsidian-dot-file-p path))))
               (not-node-git-p (not (string-match-p (rx (or "node_modules" ".git")) path)))
               (not-trash-p (obsidian-not-trash-p path))
               (not-ignored-dir (obsidian-not-in-excluded-directory-p path))
               (not-dot-obsidian (obsidian-not-dot-obsidian-p path)))
    t))

(defun obsidian-file-relative-name (f)
  "Take file name F and return relative path for `obsidian-directory'.

The call to `substring' is much faster than a call to `file-relative-name',
and as the DIRECTORY argument of `file-relative-name' is always the constant
`obsidian-directory', the use of `substring' with the FROM argument set to the
string length of `obsidian-directory' should be equivalent, as long as F is
always a full absolute path."
  (if (s-starts-with-p obsidian-directory f)
      (substring f obsidian--relative-path-length)
    f))

(defun obsidian-expand-file-name (f)
  "Take file F relative to `obsidian-directory' and return absolute path."
  (expand-file-name f obsidian-directory))

(defun obsidian-file-to-absolute-path (file)
  "Return a full file path for FILE.
The full file path is determined by finding a file with the same name in the
vault cache.  If there are multiple files with the same name, the first one
found is returned.  If no matches are found, the original FILE is returned."
  (let* ((all-files (->> (obsidian-files) (-map #'obsidian-file-relative-name)))
         (matches (obsidian--match-files file all-files)))
    (if matches
        (obsidian-expand-file-name (car matches))
      file)))

(defun obsidian-files ()
  "Lists all Obsidian Notes files that are not in trash."
  (when obsidian-vault-cache
    (hash-table-keys obsidian-vault-cache)))

(defun obsidian-directories ()
  "Lists all Obsidian sub folders."
  (->> (directory-files-recursively obsidian-directory "" t)
       (-filter #'obsidian-user-directory-p)))

(defun obsidian-remove-front-matter-from-string (s)
  "Return S with any front matter removed, returning only the body."
  (if (s-starts-with-p "---" s)
      (let ((splits (s-split-up-to "---" s 2)))
        (if (eq (length splits) 3)
            (string-trim-left (nth 2 splits))
          s))
    s))

(defun obsidian--process-front-matter-tags (front-matter &optional file)
  "Retrun list of tags from FRONT-MATTER.  FILE is used only for error messages.

FRONT-MATTER is the hashmap from `obsidian-find-yaml-front-matter-in-string'.

This function filters invalid tags (eg tags that are not in a list, or tags
that already have hashtags as these are not allowed in front matter, or
values of :null as may be returned by the YAML parser), trims whitespace,
and concatenates a hashtag to the beginning of each valid tag.

Further work could be done to attempt to extract tags from improperly
formatted front matter; however, Obsidian Notes would treat this as
improper front matter, so it is treatly similarly here while sending
a message regarding the formatting issue."
  (when front-matter
    (let* ((tags (gethash 'tags front-matter)))
      (when tags
        ;; tags in front matter should be specified as a list, not as a single string
        (if (equal 'string (type-of tags))
            (obsidian--message "Tags in front matter must be a list" file)
          (if (equal tags :null)
              (obsidian--message "The key 'tags' cannot have an empty value in front matter" file)
            (let ((resp (->> tags
                             ;; spaces are not allowed in tags; use commas between tags
                             (seq-remove (lambda (tag) (s-contains-p " " tag)))
                             ;; tags in front matter can't start with a hashtag
                             (seq-remove (lambda (tag) (s-starts-with-p "#" tag))))))
              (when (not (= (length tags) (length resp)))
                (obsidian--message "Found invalid tags in front matter" file))
              resp)))))))

(defun obsidian--process-body-tags (tags)
  "Return list of TAGS with leading whitespace and hashtag removed."
  (when tags
    (->> tags
         (seq-map #'string-trim-left)
         (seq-map (lambda (tag) (s-replace-regexp "^#" "" tag))))))

(defun obsidian-find-tags-in-string (s &optional filename)
  "Retrieve list of #tags from string S. FILENAME is used only for error messages.

First searches for front matter to find tags there, then searches through
the entire string."
  (condition-case nil
      (let* ((front-matter (obsidian-find-yaml-front-matter-in-string s))
             (fm-tags (obsidian--process-front-matter-tags front-matter filename))
             (s-body (obsidian-remove-front-matter-from-string s))
             (body-tags-raw (-flatten (s-match-strings-all obsidian--tag-regex s-body)))
             (body-tags (obsidian--process-body-tags body-tags-raw)))
        (-flatten (append fm-tags body-tags)))
    (error
     (obsidian--message "Error parsing front matter yaml for tags" filename))))

(defun obsidian-find-aliases-in-string (s &optional filename)
  "Retrieve list of aliases from string S. FILENAME is used only for error messages."
  (condition-case nil
      (when-let ((front-matter (obsidian-find-yaml-front-matter-in-string s)))
        (let* ((aliases-val (gethash 'aliases front-matter))
               ;; yaml parser can return a value of :null
               (aliases (if (equal :null aliases-val)
                            (obsidian--message "Front matter cannot have keys without values" filename)
                          aliases-val))
               (alias (gethash 'alias front-matter))
               (all-aliases (append aliases (list alias))))
          (seq-map (lambda (a) (prin1-to-string a t)) (-distinct (-filter #'identity all-aliases)))))
    (error
     (obsidian--message "Error parsing front matter yaml for aliases" filename))))

(defun obsidian--update-file-links-dict (filepath link-info dict)
  "Add LINK-INFO to value of DICT for key FILEPATH.
The value will be a nested list that contains link-info.  The nested list
will be created if necessary."
  (if-let (links-list (ht-get dict filepath))
      (ht-set dict filepath (nconc links-list (list link-info)))
    (ht-set dict filepath (list link-info)))
  dict)

(defun obsidian-find-links ()
  "Retrieve hashtable of inline links and wiki links in current buffer.

Values of hashtabale are lists with values that matche those returned by
markdown-link-at-pos:
  0. beginning position
  1. end position
  2. link text
  3. URL
  4. reference label
  5. title text
  6. bang (nil or \"!\")"
  (let ((dict (make-hash-table :test 'equal)))
    ;; Don't search for links in a near-empty file
    (when (> (point-max) 5)
      ;; Find markdown inline links
      (goto-char (point-min))
      (while (and (> (point-max) (point))
                  (markdown-match-generic-links (point-max) nil))
        (let ((link-info (markdown-link-at-pos (point))))
          (when (and (nth 2 link-info) (nth 3 link-info))
            (substring-no-properties (nth 2 link-info))
            (substring-no-properties (nth 3 link-info))
            (obsidian--update-file-links-dict
             (obsidian-file-to-absolute-path (nth 3 link-info)) link-info dict))))
      ;; Find wiki links
      (when markdown-enable-wiki-links
        (goto-char (point-min))
        (while
            (when-let (link-info (and (> (point-max) (point))
                                      (obsidian-find-wiki-links (point-max))))
              (obsidian--update-file-links-dict
               (obsidian-file-to-absolute-path
                (obsidian--extension (nth 3 link-info)))
               link-info dict)))))
    dict))

(defun obsidian-find-yaml-front-matter-in-string (s)
  "Return YAML front matter if it exists in string section S."
  (if (s-starts-with-p "---" s)
      (let* ((split (s-split-up-to "---" s 2))
             (looks-like-yaml-p (eq (length split) 3)))
        (if looks-like-yaml-p
            ;; This will throw and exception if, for example, a tag in the
            ;; front matter tag list starts with a hashtag
            (yaml-parse-string (nth 1 split))))))

(defun obsidian-tags-hashtable ()
  "Hashtable with each tags as the keys and list of file path as the values."
  (when obsidian-vault-cache

    (let ((obsidian--tags-map (make-hash-table :test 'equal)))
      ;; loop through files cache to get file/tag list for each file
      (maphash (lambda (file meta)
                 (let ((obsidian--file-metadata meta))
                   ;; loop through the tags list
                   (seq-map (lambda (rawtag)
                              (let ((tag (s-downcase rawtag)))
                                ;; Add the current file to the response list
                                ;; for the current tag in the response hash table
                                (if-let ((file-list (gethash tag obsidian--tags-map)))
                                    (progn
                                      (push (obsidian-file-relative-name file) file-list)
                                      (puthash tag file-list obsidian--tags-map))
                                  (puthash tag (list (obsidian-file-relative-name file))
                                           obsidian--tags-map))))
                            (gethash 'tags obsidian--file-metadata))))
               obsidian-vault-cache)
      (maphash (lambda (k v)
                 (puthash k (-sort 'string-lessp (-distinct v)) obsidian--tags-map))
               obsidian--tags-map)
      obsidian--tags-map)))

(defun obsidian-tags ()
  "List of Obsidian Notes tags generated by obsidian.el.
Tags in the list will NOT have a leading hashtag (#)."
  (when obsidian-vault-cache
    (-distinct
     (remove nil
             (-mapcat (lambda (val-map)
                        (gethash 'tags val-map))
                      (hash-table-values obsidian-vault-cache))))))

(defun obsidian--buffer-metadata (&optional parent-file)
  "Find the tags, aliases, and links in the current buffer and return as hashtable.
PARENT-FILE is only used for error messages."
  (save-excursion
    (let* ((bufname (or (buffer-file-name) parent-file))
           (bufstr (buffer-substring-no-properties (point-min) (point-max)))
           (tags (obsidian-find-tags-in-string bufstr bufname))
           (aliases (obsidian-find-aliases-in-string bufstr bufname))
           (links (obsidian-find-links))
           (meta (make-hash-table :test 'equal :size 3)))
      (puthash 'tags tags meta)
      (puthash 'aliases aliases meta)
      (puthash 'links links meta)
      meta)))

(defun obsidian-file-metadata (&optional file)
  "Find the tags, aliases, and links in FILE and return as hashtable.

Uses current buffer if file is not specified"
  (if (and file (file-exists-p file))
      (with-temp-buffer
        (insert-file-contents file)
        (obsidian--buffer-metadata file))
    (obsidian--buffer-metadata (buffer-file-name))))

(defun obsidian-update-file-metadata (&optional file)
  "Update the metadata for the file FILE.

If file is not specified, the current buffer will be used."
  (-let* ((filename (or file (buffer-file-name)))
          (meta (obsidian-file-metadata filename)))
    (obsidian--set-tags filename (gethash 'tags meta))
    (obsidian--set-aliases filename (gethash 'aliases meta))
    (obsidian--set-links filename (gethash 'links meta))))

(defun obsidian-enable-minor-mode ()
  "Check if current buffer is an `obsidian-file-p' and toggle `obsidian-mode'."
  (and (derived-mode-p 'markdown-mode)
       (obsidian-file-p)
       (obsidian-mode t)))

(defun obsidian--files-on-disk()
  "Return a list of all obsidian files in the vault directory."
  (let ((file-paths (directory-files-recursively obsidian-directory "\.*$")))
    (-filter #'obsidian-file-p file-paths)))

(defun obsidian-rescan-buffer ()
  "Update vault metadata for current buffer."
  (interactive)
  (let ((file (buffer-file-name)))
    (when (obsidian-file-p file)
      (obsidian-add-file file))))

(defun obsidian-rescan-cache ()
  "Create an empty cache and populate with files, tags, aliases, and links."
  (interactive)
  ;; This is used to ensure that obsidian-directory was properly initialized
  (customize-set-variable 'obsidian-directory obsidian-directory)
  (let* ((obs-files (obsidian--files-on-disk))
         (file-count (length obs-files)))
    ;; Clear existing metadata
    (setq obsidian--aliases-map (make-hash-table :test 'equal))
    (setq obsidian--backlinks-alist (make-hash-table :test 'equal))
    (setq obsidian--jump-list nil)
    (setq obsidian-vault-cache (make-hash-table :test 'equal :size file-count))
    (seq-map (lambda (file)
               (ht-set obsidian-vault-cache file (make-hash-table :test 'equal :size 3)))
             obs-files)
    ;; Repopulate metadata
    (dolist-with-progress-reporter
        (i obs-files)
        (format "Adding %d files to vault cache... " file-count)
      (obsidian-add-file i))
    (message "Obsidian cache populated at %s with %d files"
             (format-time-string "%H:%M:%S") file-count)
    (setq obsidian--updated-time (float-time))
    file-count))

(defun obsidian--updated-externally-p (file)
  "Has FILE been modified by a process other than obsidian.el."
  (let ((file-mod-time (float-time (nth 5 (file-attributes file)))))
    ;; Has the file been modified more recently than obsidian--updated-time
    (> file-mod-time obsidian--updated-time)))

;;;###autoload
(defun obsidian-update ()
  "Check the cache against files on disk and update cache as necessary.

If a file has been modified more recently than `obsidian--updated-time',
we assume it may have been modified outside of obsidian.el so we call
`obsidian-add-file'.  Note that files modified by obsidian.el would also
show more recent modified times if they called `obsidian--update-on-save'
that was triggered by the `after-save-hook'.  We have no way to distinguish
this from a file modified outside of obsidian.el, so we'll re-process
them all just in case."
  (interactive)
  (if (or (not (boundp 'obsidian-vault-cache)) (not obsidian-vault-cache))
      (obsidian-rescan-cache)
    (-let* ((cached (obsidian-files))
            (ondisk (obsidian--files-on-disk))
            (new-files (-difference ondisk cached))
            (old-files (-difference cached ondisk))
            (to-reprocess (seq-filter #'obsidian--updated-externally-p ondisk)))
      (seq-map #'obsidian-add-file new-files)
      (seq-map #'obsidian-remove-file old-files)
      (seq-map #'obsidian-add-file to-reprocess)
      (setq obsidian--updated-time (float-time))
      (when obsidian-debug-messages
        (when to-reprocess
          (message "Reprocesed the following files:\n%s" (pp to-reprocess)))
        (message "Obsidian cache updated at %s" (format-time-string "%H:%M:%S"))))))

(defun obsidian-format-link (file-path &optional toggle)
  "Return FILE-PATH in as link based on `obsidian-links-use-vault-path'.

Will format FILE-PATH based on `obsidian-links-use-vault-path' and an optional
prefix argument TOGGLE.  If link contains a colon (:), it is assumed to not be
an Obsidian link and is returned unmodified."
  (if (s-contains-p ":" file-path)
      file-path
    (if obsidian-links-use-vault-path
        (if toggle (file-name-nondirectory file-path) file-path)
      (if toggle file-path (file-name-nondirectory file-path)))))

(defun obsidian--verify-link (f)
  "Check that relative file path F exists, and create it if it does not.

Returns a file path relative to the obsidian vault."
  ;; Assume an external link that should be returned untouched
  (if (s-contains-p ":" f)
      f
    ;; (let* ((obs-path (obsidian-expand-file-name f)))
    (let* ((obs-path (obsidian-file-to-absolute-path f)))
      (if (ht-get obsidian-vault-cache obs-path)
          ;; associated file is in cache; return relative file path f
          f
        ;; file is not being tracked; create it if necessary
        (obsidian-file-relative-name
         (obsidian--prepare-new-file-from-rel-path f))))))

(defun obsidian--request-link (&optional toggle-path)
  "Service function to request user for link input.

TOGGLE-PATH is a boolean that will toggle the behavior of
`obsidian-links-use-vault-path' for this single link insertion."
  (let* ((all-files (->> (obsidian-files)
                         (-map (lambda (f) (file-relative-name f obsidian-directory)))))
         (region (when (use-region-p)
                   (buffer-substring-no-properties (region-beginning) (region-end))))
         (chosen-file (completing-read "Link: " all-files))
         (verified-file (obsidian--verify-link chosen-file))
         (default-description (-> verified-file
                                  file-name-nondirectory
                                  file-name-sans-extension))
         (description (->> (or region default-description)
                           (read-from-minibuffer "Description (optional): ")))
         (file-link (obsidian-format-link verified-file toggle-path)))
    (list :file file-link :description description)))

;;;###autoload
(defun obsidian-insert-wikilink (&optional arg)
  "Insert a link to file in wikilink format.

If ARG is set, the value of `obsidian-links-use-vault-path' will be toggled for
the current link insertion."
  (interactive "P")
  (let* ((file (obsidian--request-link arg))
         (filename (plist-get file :file))
         (description (plist-get file :description))
         (no-ext (file-name-sans-extension filename))
         (link (if (and description (not (s-ends-with-p description no-ext)))
                   (if obsidian-wiki-link-alias-first
                       (s-concat "[[" description "|" no-ext "]]")
                     (s-concat "[[" no-ext "|" description"]]"))
                 (s-concat "[[" no-ext "]]"))))
    (insert link)))

;;;###autoload
(defun obsidian-insert-link (&optional arg)
  "Insert a link to file in markdown format.

If ARG is set, the value of `obsidian-links-use-vault-path' will be toggled for
this link insertion.  If text is highlighted, the highlighted text will be
replaced by the link."
  (interactive "P")
  (let* ((file-plist (obsidian--request-link arg))
         (file-raw (plist-get file-plist :file))
         (file (s-replace " " "%20" file-raw))
         (description (plist-get file-plist :description))
         (link-str (s-concat "[" description "](" file ")")))
    (if (use-region-p)
        (delete-active-region))
    (insert link-str)))

;;;###autoload
(defun obsidian-remove-link ()
  "Remove link and replace with link text."
  (interactive)
  (cond ((markdown-link-p)
         (let ((link (markdown-link-at-pos (point))))
           (delete-region (nth 0 link) (nth 1 link))
           (insert (or (nth 2 link) (nth 3 link)))))
        ((markdown-wiki-link-p)
         (markdown-kill-thing-at-point)
         (yank))))

(defun obsidian--prepare-tags-list (tags)
  "Prepare a list of TAGS with both lower-case and capitalized versions.

Obsidian Notes tags are case-independent and are therefore considered to be
the same no matter their case.  Sometimes it's convenient to capitalize a
tag, for example when using it at the start of the sentence.  This function
allows completion with both lower and upper case versions of the tags."
  (let* ((lower-case (-map #'s-downcase tags))
         (capitalized (-map #'s-capitalize lower-case))
         (merged (-concat tags lower-case capitalized)))
    (-distinct merged)))

(defun obsidian--tags-backend (command &rest arg)
  "Completion backend for company used by obsidian.el.
Argument COMMAND company command.
Optional argument ARG word to complete."
  (interactive (if (and (featurep 'company)
                        (fboundp 'company-begin-backend))
                   (company-begin-backend 'obsidian--tags-backend)
                 (error "Company not installed")))
  (cl-case command
    (prefix (and (eq major-mode 'markdown-mode)
                 (-contains-p local-minor-modes 'obsidian-mode)
                 (fboundp 'company-grab-symbol)
                 (company-grab-symbol)))
    (candidates (->> (obsidian-tags)
                     obsidian--prepare-tags-list
                     (-filter (lambda (s) (s-starts-with-p (car arg) s)))))))

(defun obsidian-point-in-front-matter-p (&optional point)
  "Return t if POINT is currently inside YAML front matter."
  (let ((point (or point (point))))
    (save-excursion
      (goto-char 0)
      (when (eq 4 (search-forward-regexp "^---" nil t))
        (goto-char 4)
        (<= point (search-forward-regexp "^---" nil t))))))

;;;###autoload
(defun obsidian-insert-tag ()
  "Insert a tag from the existing tags."
  (interactive)
  (let* ((tags (-sort #'string< (obsidian-tags)))
         (choice (completing-read "Insert tag: " tags))
         (fm (obsidian-point-in-front-matter-p (point)))
         (tag (if fm
                  choice
                (format "#%s" choice))))
    (insert tag)))

;;;###autoload
(defun obsidian-capture ()
  "Create new obsidian note.

In the `obsidian-inbox-directory' if set otherwise in `obsidian-directory' root."
  (interactive)
  (let* ((title (read-from-minibuffer "Title: "))
         (filename (s-concat obsidian-directory "/" obsidian-inbox-directory "/" title ".md"))
         (clean-filename (s-replace "//" "/" filename)))
    (find-file (expand-file-name clean-filename) t)
    (save-buffer)))

;;;###autoload
(defun obsidian-daily-note ()
  "Create new obsidian daily note.

Note is created in the `obsidian-daily-notes-directory' if set, or in
`obsidian-inbox-directory' if set, or finally n `obsidian-directory' root."
  (interactive)
  (let* ((title (format-time-string "%Y-%m-%d"))
         (filename (s-concat obsidian-directory "/" obsidian-daily-notes-directory "/" title ".md"))
         (clean-filename (s-replace "//" "/" filename)))
    (find-file (expand-file-name clean-filename) t)
    (save-buffer)
    (when (and obsidian-templates-directory
               obsidian-daily-note-template
               (eq (buffer-size) 0))
      (obsidian-apply-template
       (s-concat obsidian-directory "/"
                 obsidian-templates-directory "/"
                 obsidian-daily-note-template))
      (save-buffer))))

;;;###autoload
(defun obsidian-jump ()
  "Jump to Obsidian note."
  (interactive)
  (let* ((files (obsidian-files))
         (dict (make-hash-table :test 'equal))
         (_ (-map (lambda (f)
                    (puthash (file-relative-name f obsidian-directory) f dict))
                  files))
         (choices (-sort #'string< (-distinct (-concat (obsidian-aliases) (hash-table-keys dict)))))
         (choice (completing-read "Jump to: " choices))
         (target (obsidian--get-alias choice (gethash choice dict))))
    (if target
        (find-file target)
      (user-error "Note not found: %s" choice))))

(defun obsidian--mapped-aliases (file)
  "Return list of aliases mapped to FILE in `obsidian--aliases-map'."
  (let ((aliases '()))
    (maphash (lambda (k v)
               (when (equal file v)
                 (add-to-list 'aliases k)))
             obsidian--aliases-map )
    aliases))

(defun obsidian-add-file (file)
  "Add a FILE to the files cache and update tags and aliases for the file."
  (let ((file (expand-file-name file)))
    (when (not (gethash file obsidian-vault-cache))
      (puthash file (make-hash-table :test 'equal :size 3) obsidian-vault-cache))
    (obsidian-update-file-metadata file)))

(defun obsidian-remove-file (file)
  "Remove FILE from the files cache and update tags and aliases accordingly."
  (let ((file (expand-file-name file)))
    (-map #'obsidian--remove-alias (obsidian--mapped-aliases file))
    (remhash file obsidian-vault-cache)))

(defun obsidian--update-on-save ()
  "Used as a hook to update the vault cache when a file is saved."
  (when (obsidian-file-p (buffer-file-name))
    (obsidian-add-file (buffer-file-name))))

(defun obsidian-vault-directories ()
  "Provide a list of the directories in the Obsidian vault."
  (let* ((dict (make-hash-table :test 'equal))
         (_ (-map (lambda (d)
                    (puthash (file-relative-name d obsidian-directory) d dict))
                  (obsidian-directories))))
    dict))

;;;###autoload
(defun obsidian-move-file ()
  "Move current note to another directory."
  (interactive)
  (when (not (obsidian-file-p (buffer-file-name)))
    (user-error "Current file is not an obsidian-file"))
  (let* ((old-file-path (buffer-file-name))
         (dict (obsidian-vault-directories))
         (choice (completing-read "Move to: " (hash-table-keys dict)))
         (new-file-directory (file-name-as-directory (gethash choice dict)))
         (new-file-path (expand-file-name (file-name-nondirectory old-file-path) new-file-directory)))
    (when (equal new-file-path old-file-path)
      (user-error "File already exists at that location"))
    (rename-file old-file-path new-file-directory)
    (write-file new-file-path)
    (obsidian-remove-file old-file-path)
    (message "Moved to %s" new-file-path)))

(defun obsidian--match-files (f all-files)
  "Filter ALL-FILES to return list with same name as F."
  (-filter (lambda (el) (or (s-equals-p f el) (s-ends-with-p (concat "/" f) el))) all-files))

(defun obsidian--prepare-new-file-from-rel-path (p)
  "Create file if it doesn't exist and return full system path for relative path P.

If the file include directories in its path, we create the file relative to
`obsidian-directory'.  If there are no paths, we create the new file in
`obsidian-inbox-directory' if `obsidian-inbox-directory' and
`obsidian-create-unfound-files-in-inbox' are set, otherwise in
`obsidian-directory'."
  (let* ((f (obsidian--extension p))
         (filename (cond
                    ;; If relative path includes a '/', use vault root
                    ((s-contains-p "/" f)
                     (s-concat obsidian-directory "/" f))
                    ;; Create file in inbox if appropriate
                    ((and obsidian-create-unfound-files-in-inbox
                          obsidian-inbox-directory)
                     (s-concat obsidian-directory "/"
                               obsidian-inbox-directory "/" f))
                    ;; If we're in a file buffer, create new file in same directory
                    (buffer-file-name
                     (let ((rel-path (-> (buffer-file-name)
                                         file-name-directory
                                         obsidian-file-relative-name
                                         (concat f))))
                       (s-concat obsidian-directory "/" rel-path)))
                    ;; Else, create in the vault root
                    (t
                     (s-concat obsidian-directory "/" f))))
         (cleaned (s-replace "//" "/" filename)))
    (when (not (f-exists-p cleaned))
      (f-mkdir-full-path (f-dirname cleaned))
      (f-touch cleaned)
      (obsidian-add-file cleaned))
    cleaned))

(defun obsidian-find-file (f &optional arg)
  "Open file F, offering a choice if multiple files match F.

If ARG is set, the file will be opened in other window."
  (let* ((all-files (seq-map #'obsidian-file-relative-name (obsidian-files)))
         (matches (obsidian--match-files f all-files))
         (file (cl-case (length matches)
                 (0 (obsidian--prepare-new-file-from-rel-path
                     (obsidian--prepare-rel-path f)))
                 (1 (car matches))
                 (t
                  (let ((choice (completing-read "Jump to: " matches)))
                    choice))))
         (path (obsidian-expand-file-name file)))
    (if arg (find-file-other-window path) (find-file path))))

(defun obsidian-find-point-in-file (f p &optional arg)
  "Open file F at point P, offering a choice if multiple files match F.

If ARG is set, the file will be opened in other window."
  (obsidian-find-file f arg)
  (goto-char p))

(defun obsidian--prepare-rel-path (f)
  "Return relative path for creating new file F.

If `/' in F, return F. Else, if `obsidian-inbox-directory' is set and
`obsidian-create-unfound-files-in-inbox' is true, return path to in inbox.
Otherwise, retrun path in same directory as current buffer."
  (if (s-contains-p "/" f)
      f
    ;; Return relative path of input file in input diretory
    (if (and obsidian-create-unfound-files-in-inbox obsidian-inbox-directory)
        (concat (file-name-directory obsidian-inbox-directory) f)
      ;; Return relative path of input file in current directory
      (-> (buffer-file-name)
          file-name-directory
          obsidian-file-relative-name
          (concat f)))))

(defun obsidian-find-wiki-links (last)
  "Match wiki links from point to LAST.

This uses some of the logic from `markdown-match-wiki-link' but returns a
more detailed response to recognized the different parts of a wiki link.
The returned list is of the same format as returned by
`markdown-match-generic-links'."
  (when (markdown-match-inline-generic markdown-regex-wiki-link last)
    (let* ((begin (match-beginning 1))
           (end (match-end 1))
           (part1 (match-string-no-properties 3))
           (part2 (match-string-no-properties 5))
           (aliasp (string-equal (match-string-no-properties 4) "|"))
           (filename (if (and aliasp markdown-wiki-link-alias-first)
                         (markdown-convert-wiki-link-to-filename part2)
                       (markdown-convert-wiki-link-to-filename part1)))
           (linktext (if aliasp
                         (if markdown-wiki-link-alias-first part1 part2)
                       filename)))
      (if (or (markdown-in-comment-p begin)
              (markdown-in-comment-p end)
              (markdown-inline-code-at-pos-p begin)
              (markdown-inline-code-at-pos-p end)
              (markdown-code-block-at-pos begin))
          (progn (goto-char (min (1+ begin) last))
                 (when (< (point) last)
                   (obsidian-find-wiki-links last)))
        ;; Mimics match-data set by markdown-match-generic-links
        (list begin end linktext filename nil nil nil)))))

(defsubst obsidian--remove-section (s)
  "Remove section S from file path.
From `filename#section' keep only the `filename'."
  (replace-regexp-in-string "#.*$" "" s))

(defun obsidian--extension (f)
  "Add extension to wiki link F if none."
  (if (file-name-extension f)
      f
    (s-concat (obsidian--remove-section f) ".md")))

(defun obsidian-follow-wiki-link-at-point (&optional arg)
  "Find Wiki Link at point.  Opens wiki links in other window if ARG is non-nil."
  (interactive "P")
  (thing-at-point-looking-at markdown-regex-wiki-link)
  (let* ((url (s-trim (if obsidian-wiki-link-alias-first
                          (or (match-string-no-properties 5)
                              (match-string-no-properties 3))
                        (match-string-no-properties 3)))))
    (if (s-contains-p ":" url)
        (browse-url url)
      (let ((prepped-path (obsidian--extension (s-replace "%20" " " url))))
        (push (point-marker) obsidian--jump-list)
        (obsidian-find-point-in-file prepped-path 0 arg)))))

(defun obsidian-follow-markdown-link-at-point (&optional arg)
  "Find and follow markdown link at point.
Opens markdown links in other window if ARG is non-nil.."
  (interactive "P")
  (let ((normalized (s-replace "%20" " " (markdown-link-url))))
    (if (s-contains-p ":" normalized)
        (browse-url normalized)
      (progn
        (push (point-marker) obsidian--jump-list)
        (obsidian-find-point-in-file normalized 0 arg )))))

(defun obsidian-follow-backlink-at-point ()
  "Open the file pointed to by the backlink and move to the linked location."
  (let* ((link (get-text-property (point) 'obsidian--file))
         (pos (get-text-property (point) 'obsidian--position)))
    (cond ((s-contains-p ":" link)
           (browse-url link))
          ((s-starts-with-p "#" link)
           (message "Doing nothing with relative link %s" link))
          (t
           (progn
             (find-file-other-window link)
             (goto-char pos))))))

(defun obsidian-backlink-p ()
  "Check if thing at point represents a backlink."
  (and (get-text-property (point) 'obsidian--file)
       (get-text-property (point) 'obsidian--position)))

;;;###autoload
(defun obsidian-jump-back ()
  "Jump backward to previous location."
  (interactive)
  (if-let ((jump-marker (pop obsidian--jump-list)))
      (progn
        (pop-to-buffer (marker-buffer jump-marker))
        (goto-char jump-marker))
    (message "No previous location to jump to.")))

;;;###autoload
(defun obsidian-follow-link-at-point (&optional arg)
  "Follow thing at point if possible, such as a reference link or wiki link.
Opens inline and reference links in a browser.  Opens wiki links
to other files in the current window, or another window if ARG is non-nil.
See `markdown-follow-link-at-point' and `markdown-follow-wiki-link-at-point'."
  (interactive "P")
  (cond ((markdown-link-p)
         (obsidian-follow-markdown-link-at-point arg))
        ((markdown-wiki-link-p)
         (obsidian-follow-wiki-link-at-point arg))
        ((obsidian-backlink-p)
         (obsidian-follow-backlink-at-point))
        ((thing-at-point-url-at-point)
         (browse-url-at-point))))

(defun obsidian--grep (re)
  "Find RE in the Obsidian vault."
  (elgrep obsidian-directory "\.md" re
          :recursive t
          :case-fold-search t
          :exclude-file-re (if obsidian-include-hidden-files "~" "^\\.\\|~")
          :exclude-dir-re ".obsidian"))

(defun obsidian--link-p (s)
  "Check if S matches any of the link regexes."
  (when s
    (or (s-matches-p obsidian-wiki-link-regex s)
        (s-matches-p obsidian-markdown-link-regex s))))

(defun obsidian-apply-template (template-filename)
  "Apply the template from TEMPLATE-FILENAME for the current buffer.
Template vars: {{title}}, {{date}}, and {{time}}"
  (let* ((title (file-name-sans-extension (file-name-nondirectory buffer-file-name)))
         (date (format-time-string "%Y-%m-%d"))
         (time (format-time-string "%H:%M:%S"))
         (m (point))
         (template-content (with-temp-buffer
                             (insert-file-contents template-filename)
                             (buffer-string)))
         (output-content (replace-regexp-in-string "{{title}}" title template-content))
         (output-content (replace-regexp-in-string "{{date}}" date output-content))
         (output-content (replace-regexp-in-string "{{time}}" time output-content)))
    (goto-char (point-min))
    (insert output-content)
    (message "Template variables replaced and inserted to the buffer")
    (goto-char m)))

(defun obsidian--backlinks-completion-fn (hmap)
  "Completion function to show file path and link text from hashmap HMAP."
  (let ((obsidian--backlinks-alist
         (-flatten
          (ht-map (lambda (k1 v1)
                    (seq-map (lambda (v2)
                               (cons (obsidian-file-relative-name k1) (nth 2 v2)))
                             v1))
                  hmap))))
    (completing-read
     "Backlinks: "
     (lambda (str pred flag)
       (if (eq flag 'metadata)
           '(metadata (annotation-function
                       lambda (str) (concat "\tlink text: " (cdr (assoc str obsidian--backlinks-alist)))))
         (all-completions str (mapcar #'car obsidian--backlinks-alist) pred))))))

(defun obsidian-backlinks (&optional file)
  "Return a hashtable of backlinks to absolute file path FILE.

The variables used for retrieving links are as follows:
  host - host file; the one that includes the links.  full path filename
  targ - file being pointed to by the host link, full path and extension
  meta - metadata hashtable that includes links, tags, and aliases
  lmap - hashmap of links from meta
  link - link target from links hashmap
  info - nested list of link info lists for target link

The files cache has the following structure:
  {filepath: {tags:    (tag list)
              aliases: (alias list)
              links:   {linkname: (link info list)}}}"
  (let* ((targ (or file (buffer-file-name)))
         (resp (make-hash-table :test 'equal)))
    (maphash
     (lambda (host meta)
       (when-let ((lmap (gethash 'links meta)))
         (maphash
          (lambda (link info)
            (when (equal link targ)
              (puthash host info resp)))
          lmap)))
     obsidian-vault-cache)
    resp))

;;;###autoload
(defun obsidian-backlink-jump (&optional file)
  "Select a backlink to this FILE and follow it."
  (interactive)
  (let ((linkmap (obsidian-backlinks file)))
    (if (> (length (hash-table-keys linkmap)) 0)
        (let ((choice (obsidian--backlinks-completion-fn linkmap)))
          (find-file (obsidian-expand-file-name choice)))
      (message "No backlinks found."))))

;;;###autoload
(defun obsidian-backlinks-window ()
  "Visit backlinks buffer if not currently active or return to previous."
  (interactive nil obsidian-mode)
  (if obsidian-backlinks-mode
      (if (equal (buffer-name) obsidian-backlinks-buffer-name)
          (progn
            (pop obsidian--jump-list)
            (select-window (get-mru-window (selected-frame) nil :not-selected)))
        (if-let ((bakbuf (get-buffer obsidian-backlinks-buffer-name)))
            (progn
              (push (point-marker) obsidian--jump-list)
              (pop-to-buffer bakbuf))
          (obsidian--populate-backlinks-buffer)))
    (obsidian-backlink-jump)))

(defun obsidian-file-title-function (file)
  "Return the title of FILE.

This is a modified version of the default xeft title function.

Recognize `title:' if set, else return the first line as title or,
if the first line is empty, return the file name as the title."
  (re-search-forward (rx "title:" (* whitespace)) nil t)
  (let ((bol (point)) title)
    (end-of-line)
    (setq title (buffer-substring-no-properties bol (point)))
    (if (equal title "")
        (file-name-base file)
      title)))

;;;###autoload
(defun obsidian-search ()
  "Search Obsidian vault for input."
  (interactive)
  (let* ((query (-> (read-from-minibuffer "Search query or regex: ")))
         (results (obsidian--grep query)))
    (message (s-concat "Found " (pp-to-string (length results)) " matches"))
    (let ((choice (completing-read "Select file: " results)))
      (obsidian-find-point-in-file choice 0))))

;;;###autoload
(defun obsidian-find-tag ()
  "Find all notes with a tag."
  (interactive)
  (let* ((taghash (obsidian-tags-hashtable))
         (tag (completing-read "Select tag: " (->> (hash-table-keys taghash)
                                                   (-sort 'string-lessp))))
         (results (gethash tag taghash))
         (choice (completing-read "Select file: " results)))
    (obsidian-find-point-in-file choice 0)))

;;
;; Vault cache update timer
;;

(defun obsidian--idle-timer ()
  "Wait until Emacs is idle to call update."
  (when (and (boundp 'obsidian-update-idle-wait))
    (run-with-idle-timer obsidian-update-idle-wait nil #'obsidian-update)))

(defun obsidian-start-update-timer ()
  "Start the background process to periodically refresh the vault cache."
  (interactive)
  (when (boundp 'obsidian-cache-expiry)
    (message "Starting obsidian update timer")
    (setq obsidian--update-timer
          (run-with-timer 0 obsidian-cache-expiry 'obsidian--idle-timer))))

(defun obsidian-stop-update-timer ()
  "Stop the background process that periodically refreshes the vault cache."
  (interactive)
  (when (and (boundp 'obsidian--update-timer) obsidian--update-timer)
    (message "Stopping obsidian update timer")
    (cancel-timer obsidian--update-timer)))

(defcustom obsidian-use-update-timer t
  "Determines whether a polling cache update will be used.
If it is true, a timer will be created using the values of
`obsidian-cache-expiry' and `obsidian-update-idle-wait'."
  :type 'boolean
  :initialize #'custom-initialize-reset
  :set (lambda (symbol value)
         (set-default symbol value)
         (if value
             (obsidian-start-update-timer)
           (obsidian-stop-update-timer))))

(defcustom obsidian-cache-expiry (* 60 5)
  "The number of seconds before the Obsidian cache will update."
  :type 'integer
  :initialize #'custom-initialize-reset
  :set (lambda (symbol value)
         (set-default symbol value)
         (obsidian-stop-update-timer)
         (obsidian-start-update-timer)))

(defcustom obsidian-update-idle-wait 5
  "Seconds to wait after cache expiry for Emacs to be idle before running update."
  :type 'integer
  :initialize #'custom-initialize-reset
  :set (lambda (symbol value)
         (set-default symbol value)
         (obsidian-stop-update-timer)
         (obsidian-start-update-timer)))

;;
;; Backlinks Panel
;;

(defcustom obsidian-backlinks-panel-position 'right
  "Position of backlinks buffer in frame.
Valid values are:
 * `right',
 * `left'."
  :type '(choice (const right)
                 (const left)))

(defcustom obsidian-backlinks-panel-width 75
  "Width of the backlinks window."
  :type 'integer)

(defcustom obsidian-backlinks-show-vault-path t
  "If t, show path relative to Obsidian vault, otherwise only show file name."
  :type 'boolean)

(defcustom obsidian-backlinks-buffer-name "*backlinks*"
  "Name to use for the obsidian backlinks buffer."
  :type 'string)

(defun obsidian--get-local-backlinks-window (&optional frame)
  "Return window if backlinks window is visible in FRAME, nil otherwise.

Inspired by `treemacs-get-local-window' in `treemacs-scope.el'."
  (let ((search-frame (or frame (selected-frame))))
    (->> (window-list search-frame)
         (--first (->> it
                       (window-buffer)
                       (buffer-name)
                       (s-starts-with? obsidian-backlinks-buffer-name))))))

(defun obsidian--backlinks-set-panel-width (width)
  "Set the width of the backlinks buffer to WIDTH.
For an interactive version, see `obsidian-backlinks-set-panel-width'."
  (unless (one-window-p)
    (let* ((bakbuf (get-buffer obsidian-backlinks-buffer-name))
           (win (get-buffer-window obsidian-backlinks-buffer-name))
           (win-width (window-width win))
           (new-width (max width window-safe-min-width)))
      (with-current-buffer bakbuf
        (setq window-size-fixed nil)
        (window-resize win (- new-width win-width) t)
        (message "> :: adjusted %d to %d (win-width: %d\tnew-width: %d)"
                 (- new-width win-width) (window-width win) win-width new-width)
        (setq window-size-fixed 'width))
      (setq obsidian-backlinks-panel-width new-width)
      (obsidian--populate-backlinks-buffer 'force))))

(defun obsidian-backlinks-set-panel-width (&optional arg)
  "Select a new value for `obsidian-backlinks-panel-width'.
With a prefix ARG simply reset the width of the treemacs window."
  (interactive "P")
  (unless arg
    (setq obsidian-backlinks-panel-width
          (->> obsidian-backlinks-panel-width
               (format "New Width (current = %s): ")
               (read-number))))
  (obsidian--backlinks-set-panel-width obsidian-backlinks-panel-width))

(defun obsidian-open-backlinks-panel ()
  "Create a dedicated panel to display the backlinks buffer.

Inspired by treemacs.  See `treemacs--popup-window' in `treemacs-core-utils.el'
for an example of using `display-buffer-in-side-window'."
  (interactive)
  (let ((bakbuf (get-buffer-create obsidian-backlinks-buffer-name)))
    (with-current-buffer bakbuf
      (setq window-size-fixed 'width))
    (display-buffer-in-side-window
     bakbuf
     `((side . ,obsidian-backlinks-panel-position)
       (window-width . ,obsidian-backlinks-panel-width)
       (slot . -1)
       (dedicated . t)))))

(defun obsidian-close-backlinks-panel ()
  "Close local window used for dedicated backlinks panel."
  (interactive)
  (delete-windows-on obsidian-backlinks-buffer-name 0))

(defun obsidian-close-all-backlinks-panels ()
  "Close all windows used for dedicated backlinks panels."
  (delete-windows-on obsidian-backlinks-buffer-name 'all)
  (balance-windows))

(defun obsidian-toggle-backlinks-panel ()
  "Create backlinks panel if it doesn't exist, close it otherwise.

Returns t if a panel was created, nil if closed."
  (interactive)
  (if (obsidian--get-local-backlinks-window)
      (progn
        (obsidian-close-backlinks-panel)
        nil)
    (progn
      (obsidian-open-backlinks-panel)
      (obsidian-backlinks-mode t)
      t)))

(defun obsidian--link-with-props (k v)
  "Create a propertized link and link text string from K and V.

K is the file name that contains the link.
V is the list object associated with the link as returned
by `markdown-link-at-pos'."
  (let ((filename (cond ((or (s-starts-with-p "#" k)
                             (s-contains-p ":" k))
                         k)
                        (obsidian-backlinks-show-vault-path
                         (obsidian-file-relative-name k))
                        (t
                         (file-name-nondirectory k)))))
    (insert (propertize (format "%s\n" filename)
                        'face 'markdown-url-face 'obsidian--file k))

    (mapcar
     (lambda (info)
       (insert (propertize (format "- %s\n" (nth 2 info))
                           'obsidian--file k
                           'obsidian--position (nth 0 info))))
     v)))

(defun obsidian-file-backlinks-displayed-p (&optional file)
  "Return t if the backlinks panel is showing the backlinks for FILE, else nil.

FILE is the full path to an obsidian file."
  (let* ((file-path (or file (buffer-file-name)))
         (bakbuf (get-buffer obsidian-backlinks-buffer-name))
         (file-prop (get-text-property 1 'obsidian-mru-file bakbuf)))
    (equal file-path file-prop)))

(defun obsidian--populate-backlinks-buffer (&optional force)
  "Populate backlinks buffer with backlinks for current Obsidian file.

The backlinks buffer will not be updated if it's already showing the
backlinks for the current buffer unless FORCE is non-nil."
  (unless (and (obsidian-file-backlinks-displayed-p) (not force))
    (when (and obsidian-mode (obsidian--get-local-backlinks-window) (obsidian-file-p))
      (let* ((file-path (buffer-file-name))
             (vault-path (obsidian-file-relative-name file-path))
             (backlinks (obsidian-backlinks file-path))
             (file-str (if obsidian-backlinks-show-vault-path
                           vault-path
                         (file-name-base file-path))))
        (with-current-buffer (get-buffer obsidian-backlinks-buffer-name)
          (erase-buffer)
          (visual-line-mode t)
          ;; Insert filename
          (insert (propertize (format "# %s\n" file-str)
                              'face 'markdown-header-face
                              'obsidian-mru-file file-path))
          ;; Insert separator
          (insert (propertize
                   (format "%s\n" (make-string (- obsidian-backlinks-panel-width 2) ?-))
                   'face 'markdown-hr-face))
          ;; Insert backlinks
          (maphash 'obsidian--link-with-props backlinks)
          ;; Allows for using keybindings for obsidian-open-link
          (obsidian-mode t)
          ;; Put cursor on the line of the first link
          (goto-char (point-min))
          (forward-line 2)
          (set-window-point
           (get-buffer-window obsidian-backlinks-buffer-name)
           (point)))))))

;;
;; Mode Configuration
;;

(when (eval-when-compile (require 'hydra nil t))
  (defhydra obsidian-hydra (:hint nil)
    "
Obsidian
_f_ollow at point   insert _w_ikilink          _q_uit
_j_ump to note      insert _l_ink              capture daily _n_ote
_t_ag find          _c_apture new note
_s_earch by expr.   _u_pdate tags/alises etc.
"
    ("c" obsidian-capture)
    ("n" obsidian-daily-note)
    ("f" obsidian-follow-link-at-point)
    ("j" obsidian-jump)
    ("l" obsidian-insert-link :color blue)
    ("q" nil :color blue)
    ("s" obsidian-search)
    ("t" obsidian-find-tag)
    ("u" obsidian-update)
    ("w" obsidian-insert-wikilink :color blue)))

;;;###autoload
(define-globalized-minor-mode global-obsidian-mode obsidian-mode obsidian-enable-minor-mode)

(add-hook 'after-save-hook #'obsidian--update-on-save)

;;;###autoload
(define-minor-mode obsidian-backlinks-mode
  "When active, open a buffer showing the backlinks for the current file.

Opening an Obsidian file will automatically create a separate
temporary buffer showing the backlinks to that file.

The backlinks themselves are links, linking back to the location pointed
in the linked file."
  :global t
  :lighter " Bk"
  (cond
   (obsidian-backlinks-mode
    ;; mode was turned on
    (obsidian-open-backlinks-panel)
    (obsidian--populate-backlinks-buffer)
    (add-hook 'buffer-list-update-hook #'obsidian--populate-backlinks-buffer)
    (if (boundp 'eyebrowse-post-window-switch-hook)
        (remove-hook 'eyebrowse-post-window-switch-hook #'obsidian-close-all-backlinks-panels)))
   (t
    ;; mode was turned off (or we refused to turn it on)
    (remove-hook 'buffer-list-update-hook #'obsidian--populate-backlinks-buffer)
    (obsidian-close-all-backlinks-panels)
    (if (boundp 'eyebrowse-post-window-switch-hook)
        (add-hook 'eyebrowse-post-window-switch-hook
                  #'obsidian-close-all-backlinks-panels)))))

(provide 'obsidian)
;;; obsidian.el ends here
