;;; xhg.el --- Mercurial interface for dvc

;; Copyright (C) 2005-2009 by all contributors

;; Author: Stefan Reichoer, <stefan@xsteve.at>

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; The mercurial interface for dvc

;;; Commands:
;;
;; Below are complete command list:
;;
;;  `xhg-init'
;;    Run hg init.
;;  `xhg-rollback'
;;    Run hg rollback.
;;  `xhg-addremove'
;;    Run hg addremove.
;;  `xhg-dvc-rename'
;;    Run hg rename.
;;  `xhg-forget'
;;    Run hg forget.
;;  `xhg-add-all-files'
;;    Run 'hg add' to add all files to mercurial.
;;  `xhg-log'
;;    Run hg log.
;;  `xhg-diff-1'
;;    Run hg diff.
;;  `xhg-dvc-diff'
;;    Run hg diff.
;;  `xhg-pull'
;;    Run hg pull.
;;  `xhg-push'
;;    Run hg push.
;;  `xhg-clone'
;;    Run hg clone.
;;  `xhg-incoming'
;;    Run hg incoming.
;;  `xhg-outgoing'
;;    Run hg outgoing.
;;  `xhg-strip'
;;    Run hg strip.
;;  `xhg-merge'
;;    Run hg merge.
;;  `xhg-resolve'
;;    Run hg resolve --all or <spec file>.
;;  `xhg-resolve-list'
;;    Run hg resolve --list.
;;  `xhg-command-version'
;;    Run hg version.
;;  `xhg-branch'
;;    Run hg branch.
;;  `xhg-branches'
;;    run xhg-branches
;;  `xhg-merge-branch'
;;    Run hg merge <branch-name>.
;;  `xhg-manifest'
;;    Run hg manifest.
;;  `xhg-tip'
;;    Run hg tip.
;;  `xhg-heads'
;;    Run hg heads.
;;  `xhg-parents'
;;    Run hg parents.
;;  `xhg-identify'
;;    Run hg identify.
;;  `xhg-verify'
;;    Run hg verify.
;;  `xhg-showconfig'
;;    Run hg showconfig.
;;  `xhg-paths'
;;    Run hg paths.
;;  `xhg-tag'
;;    Run hg tag -r <REV> NAME.
;;  `xhg-tags'
;;    Run hg tags.
;;  `xhg-view'
;;    Run hg view.
;;  `xhg-export'
;;    Run hg export.
;;  `xhg-import'
;;    Run hg import.
;;  `xhg-undo'
;;    Run hg undo.
;;  `xhg-update'
;;    Run hg update.
;;  `xhg-convert'
;;    Convert a foreign SCM repository to a Mercurial one.
;;  `xhg-serve'
;;    Run hg serve --daemon.
;;  `xhg-serve-kill'
;;    Kill a hg serve process started with `xhg-serve'.
;;  `xhg-revision-get-last-or-num-revision'
;;    Run the command:
;;  `xhg-ediff-file-at-rev'
;;    Ediff file at rev1 against rev2.
;;  `xhg-missing-1'
;;    Shows the logs of the new arrived changesets after a pull and before an update.
;;  `xhg-save-diff'
;;    Save the current hg diff to a file named FILENAME.
;;  `xhg-hgrc-edit-repository-hgrc'
;;    Edit the .hg/hgrc file for the current working copy
;;  `xhg-hgrc-edit-global-hgrc'
;;    Edit the ~/.hgrc file
;;  `hgrc-mode-help'
;;    Show the manual for the hgrc configuration file.
;;
;;; Customizable Options:
;;
;; Below are customizable option list:
;;

;;; History:

;;

;;; Code:

(require 'dvc-core)
(require 'dvc-diff)
(require 'xhg-core)
(require 'xhg-log)
(require 'xhg-mq)
(require 'xhg-annotate)

(defvar xhg-export-git-style-patches t "Run hg export --git.")

;;;###autoload
(defun xhg-init (&optional dir)
  "Run hg init."
  (interactive
   (list (expand-file-name (dvc-read-directory-name "Directory for hg init: "
                                                    (or default-directory
                                                        (getenv "HOME"))))))
  (dvc-run-dvc-sync 'xhg (list "init" dir)
                    :finished (dvc-capturing-lambda
                                  (output error status arguments)
                                (message "hg init %s finished" dir))))

;;;###autoload
(defun xhg-dvc-add-files (&rest files)
  "Run hg add."
  (dvc-trace "xhg-add-files: %s" files)
  (let ((default-directory (xhg-tree-root)))
    (dvc-run-dvc-sync 'xhg (append '("add") (mapcar #'file-relative-name files))
                      :finished (dvc-capturing-lambda
                                    (output error status arguments)
                                  (message "hg add finished")))))

;;;###autoload
(defun xhg-dvc-revert-files (&rest files)
  "Run hg revert."
  (dvc-trace "xhg-revert-files: %s" files)
  (let ((default-directory (xhg-tree-root)))
    (dvc-run-dvc-sync 'xhg (append '("revert") (mapcar #'file-relative-name files))
                      :finished (dvc-capturing-lambda
                                    (output error status arguments)
                                  (message "hg revert finished")))))

(defun xhg-dry-tip ()
  "Extract only the revision number of tip"
  (let ((revision (with-temp-buffer
                    (apply #'call-process "hg" nil t nil
                           '("tip" "--template" "#rev#"))
                    (buffer-string))))
    revision))

;;;###autoload
(defun xhg-rollback (&optional revert)
  "Run hg rollback.
if prefix-arg (C-u) run hg revert"
  (interactive "P")
  (let ((act-rev (xhg-dry-tip))
        (new-rev))
    (if (yes-or-no-p (format "Really rollback rev %s?" act-rev))
        (progn
          (dvc-run-dvc-sync 'xhg (list "rollback")
                            :finished
                            (lambda (output error status arguments)
                              (setq new-rev (xhg-dry-tip))
                              (message
                               (when (equal act-rev new-rev)
                                 "no rollback information available"))))
          (if (and current-prefix-arg
                   (not (equal act-rev new-rev)))
              (progn
                (dvc-run-dvc-sync 'xhg (list "revert" "--all")
                                  :finished
                                  (lambda (output error status arguments)
                                    (message "hg revert finished, now at rev %s" new-rev))))
            (when (not (equal act-rev new-rev))
              (message
               "hg rollback finished, tip is now at %s don't forget to revert" new-rev))))
      (message "hg rollback aborted"))))

;;;###autoload
(defun xhg-dvc-remove-files (&rest files)
  "Run hg remove."
  (dvc-trace "xhg-remove-files: %s" files)
  (let ((default-directory (xhg-tree-root)))
    (dvc-run-dvc-sync 'xhg (append '("remove") (mapcar #'file-relative-name files))
                      :finished (dvc-capturing-lambda
                                    (output error status arguments)
                                  (message "hg remove finished")))))

;;;###autoload
(defun xhg-addremove ()
  "Run hg addremove."
  (interactive)
  (dvc-run-dvc-sync 'xhg '("addremove")
                    :finished (dvc-capturing-lambda
                                  (output error status arguments)
                                (message "hg addremove finished"))))

;;;###autoload
(defun xhg-dvc-rename (from to &optional after force)
  "Run hg rename."
  (interactive
   (let* ((from-name (dvc-confirm-read-file-name "xhg rename: "))
          (to-name (dvc-confirm-read-file-name (concat "xhg rename '" from-name "' to: ") nil "" from-name)))
     (list from-name to-name nil nil)))
  (dvc-run-dvc-sync 'xhg (list "rename" (dvc-uniquify-file-name from) (dvc-uniquify-file-name to)
                               (when after "--after") (when force "--force"))
                    :finished (dvc-capturing-lambda
                                  (output error status arguments)
                                (message "hg rename finished"))))

;;;###autoload
(defun xhg-forget (&rest files)
  "Run hg forget."
  (interactive (dvc-current-file-list))
  (let ((multiprompt (format "Forget %%d files for hg? "))
        (singleprompt (format "Forget file for hg: ")))
    (when (dvc-confirm-read-file-name-list multiprompt files singleprompt t)
      (dvc-run-dvc-sync 'xhg (append '("forget") files)
                        :finished (dvc-capturing-lambda
                                      (output error status arguments)
                                    (message "hg forget finished"))))))

;;;###autoload
(defun xhg-add-all-files (arg)
  "Run 'hg add' to add all files to mercurial.
Normally run 'hg add -n' to simulate the operation to see which files will be added.
Only when called with a prefix argument, add the files."
  (interactive "P")
  (dvc-run-dvc-sync 'xhg (list "add" (unless arg "-n"))))

;;;###autoload
(defun xhg-log-toggle-verbose ()
  (interactive)
  (if xhg-log-verbose
      (progn
        (setq xhg-log-verbose nil)
        (apply #'xhg-log
               xhg-log-remember-func-args))
      (setq xhg-log-verbose t)
      (apply #'xhg-log
             xhg-log-remember-func-args)))

(defvar xhg-log-verbose nil)
(defvar xhg-log-remember-last-args nil)
(defvar xhg-log-remember-func-args nil)
;;;###autoload
(defun xhg-log (&optional r1 r2 show-patch file)
  "Run hg log.
When run interactively, the prefix argument decides, which parameters are queried from the user.
C-u      : Show patches also, use all revisions
C-u C-u  : Show patches also, ask for revisions
positive : Don't show patches, ask for revisions.
negative : Don't show patches, limit to n revisions."
  (interactive "P")
  (when (interactive-p)
    (cond ((equal current-prefix-arg '(4))
           (setq show-patch t)
           (setq r1 nil))
          ((equal current-prefix-arg '(16))
           (setq show-patch t)
           (setq r1 1)))
    (when (and (numberp r1) (> r1 0))
      (setq r1 (read-string "hg log, R1:"))
      (setq r2 (read-string "hg log, R2:"))))
  (let ((buffer (dvc-get-buffer-create 'xhg 'log))
        (command-list '("log"))
        (cur-dir default-directory))
    (when r1
      (when (numberp r1)
        (setq r1 (number-to-string r1))))
    (when r2
      (when (numberp r2)
        (setq r2 (number-to-string r2))))
    (if (and (> (length r2) 0) (> (length r1) 0))
        (setq command-list (append command-list (list "-r" (concat r2 ":" r1))))
      (when (> (length r1) 0)
        (let ((r1-num (string-to-number r1)))
          (if (> r1-num 0)
              (setq command-list (append command-list (list "-r" r1)))
            (setq command-list
                  (append command-list
                          (list "-l" (number-to-string (abs r1-num)))))))))
    (when show-patch
      (setq command-list (append command-list (list "-p"))))
    ;; be verbose or not
    (setq xhg-log-remember-last-args command-list)
    (setq xhg-log-remember-func-args (list r1 r2 show-patch file))
    (if (and xhg-log-remember-last-args
             xhg-log-verbose)
        (setq command-list (append '("-v") xhg-log-remember-last-args))
        (setq command-list xhg-log-remember-last-args))
    (dvc-switch-to-buffer-maybe buffer)
    (let ((inhibit-read-only t))
      (erase-buffer))
    (xhg-log-mode)
    ;;(dvc-trace "xhg-log command-list: %S, default-directory: %s" command-list cur-dir)
    (let ((default-directory cur-dir))
      (dvc-run-dvc-sync 'xhg command-list
                        :finished
                        (dvc-capturing-lambda (output error status arguments)
                          (progn
                            (with-current-buffer (capture buffer)
                              (let ((inhibit-read-only t))
                                (erase-buffer)
                                (insert-buffer-substring output)
                                (goto-char (point-min))
                                (insert (format "hg log for %s\n\n" default-directory))
                                (toggle-read-only 1)))))))))

(defun xhg-parse-diff (changes-buffer)
  (save-excursion
    (while (re-search-forward
            "^diff -r [^ ]+ \\(.*\\)$" nil t)
      (let* ((name (match-string-no-properties 1))
             (added (progn (forward-line 1)
                           (looking-at "^--- /dev/null")))
             (removed (progn (forward-line 1)
                             (looking-at "^\\+\\+\\+ /dev/null"))))
        (with-current-buffer changes-buffer
          (ewoc-enter-last
           dvc-fileinfo-ewoc
           (make-dvc-fileinfo-legacy
            :data (list 'file
                        name
                        (cond (added   "A")
                              (removed "D")
                              (t " "))
                        (cond ((or added removed) " ")
                              (t "M"))
                        " "             ; dir. Nothing is a directory in hg.
                        nil))))))))

(defun xhg-parse-status (changes-buffer)
  (let ((status-list (split-string (dvc-buffer-content (current-buffer)) "\n")))
    (let ((inhibit-read-only t)
          (modif)
          (modif-char))
      (erase-buffer)
      (setq dvc-header (format "hg status for %s\n" default-directory))
      (dolist (elem status-list)
        (unless (string= "" elem)
          (setq modif-char (substring elem 0 1))
          (with-current-buffer changes-buffer
            (ewoc-enter-last
             dvc-fileinfo-ewoc
             (make-dvc-fileinfo-legacy
              :data (list 'file (substring elem 2) modif-char)))))))))

(defun xhg-diff-1 (modified path dont-switch base-rev)
  "Run hg diff.
If DONT-SWITCH, don't switch to the diff buffer"
  (interactive (list nil nil current-prefix-arg))
  (let* ((window-conf (current-window-configuration))
         (cur-dir (or path default-directory))
         (orig-buffer (current-buffer))
         (root (xhg-tree-root cur-dir))
         (buffer (dvc-prepare-changes-buffer
                  `(xhg (last-revision ,root 1))
                  `(xhg (local-tree ,root))
                  'diff root 'xhg))
         (command-list '("diff")))
    (dvc-switch-to-buffer-maybe buffer)
    (dvc-buffer-push-previous-window-config window-conf)
    (when dont-switch (pop-to-buffer orig-buffer))
    (dvc-save-some-buffers root)
    (when base-rev
      (setq command-list (append command-list (list "-r" base-rev)))
      (when modified
        (setq command-list (append command-list (list "-r" modified)))))
    (dvc-run-dvc-sync 'xhg command-list
                      :finished
                      (dvc-capturing-lambda (output error status arguments)
                        (dvc-show-changes-buffer output 'xhg-parse-diff
                                                 (capture buffer))))))

;;;###autoload
(defun xhg-dvc-diff (&optional base-rev path dont-switch)
  "Run hg diff.
If DONT-SWITCH, don't switch to the diff buffer"
  (interactive (list nil nil current-prefix-arg))
  (xhg-diff-1 nil path dont-switch
              (dvc-revision-to-string base-rev nil "tip")))

(defun xhg-delta (base-rev modified &optional path dont-switch)
  ;; TODO: dvc-revision-to-string doesn't work for me.
  (interactive (list nil nil nil current-prefix-arg))
  (xhg-diff-1 (dvc-revision-to-string modified) path dont-switch
              (dvc-revision-to-string base-rev)))

(defun xhg-dvc-status ()
  "Run hg status."
  (let* ((window-conf (current-window-configuration))
         (root (xhg-tree-root))
         (buffer (dvc-prepare-changes-buffer
                  `(xhg (last-revision ,root 1))
                  `(xhg (local-tree ,root))
                  'status root 'xhg)))
    (dvc-switch-to-buffer-maybe buffer)
    (dvc-buffer-push-previous-window-config window-conf)
    (dvc-save-some-buffers root)
    (dvc-run-dvc-sync 'xhg '("status")
                      :finished
                      (dvc-capturing-lambda (output error status arguments)
                        (with-current-buffer (capture buffer)
                          (xhg-status-extra-mode-setup)
                          (if (> (point-max) (point-min))
                              (dvc-show-changes-buffer output 'xhg-parse-status
                                                       (capture buffer))
                            (dvc-diff-no-changes (capture buffer)
                                                 "No changes in %s"
                                                 (capture root))))))))

(easy-menu-define xhg-mode-menu dvc-diff-mode-map
  "`xhg' menu"
  `("hg"
    ,xhg-mq-submenu
    ["Edit project hgrc file" xhg-hgrc-edit-repository-hgrc t]
    ["Edit global ~/.hgrc file" xhg-hgrc-edit-global-hgrc t]
    ))

(defun xhg-status-extra-mode-setup ()
  "Do some additonal setup for xhg status buffers."
  (dvc-trace "xhg-status-extra-mode-setup called.")
  (easy-menu-add xhg-mode-menu)
  (when (boundp 'xhg-mq-sub-mode-map)
    (local-set-key [?Q] xhg-mq-sub-mode-map))
  (setq dvc-buffer-refresh-function 'xhg-dvc-status))

(defun xhg-pull-finish-function (output error status arguments)
  (let ((buffer (dvc-get-buffer-create 'xhg 'pull)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert-buffer-substring output)
        (toggle-read-only 1)))
    (let ((dvc-switch-to-buffer-mode 'show-in-other-window))
      (dvc-switch-to-buffer buffer))))

;;;###autoload
(defun xhg-pull (src &optional update-after-pull)
  "Run hg pull."
  (interactive (list (let* ((completions (xhg-paths 'both))
                            (initial-input (car (member "default" completions))))
                       (dvc-completing-read
                        "Pull from hg repository: "
                        completions nil nil initial-input))))
  (dvc-run-dvc-async 'xhg (list "pull" (when update-after-pull "--update") src)
                     :error 'xhg-pull-finish-function
                     :finished 'xhg-pull-finish-function))

(defun xhg-push-finish-function (output error status arguments)
  (let ((buffer (dvc-get-buffer-create 'xhg 'push)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert-buffer-substring output)
        (toggle-read-only 1)))
    (let ((dvc-switch-to-buffer-mode 'show-in-other-window))
      (dvc-switch-to-buffer buffer))))

;;;###autoload
(defun xhg-push (src)
  "Run hg push."
  (interactive (list (let* ((completions (xhg-paths 'both))
                            (initial-input (car (member "default" completions))))
                       (dvc-completing-read
                        "Push to hg repository: "
                        completions nil nil initial-input))))
  (dvc-run-dvc-async 'xhg (list "push" src)
                     :error 'xhg-push-finish-function
                     :finished 'xhg-push-finish-function))

;;;###autoload
(defun xhg-clone (src &optional dest rev noupdate pull)
  "Run hg clone."
  (interactive (list (read-string "hg clone from: ")
                     (read-string "hg clone to: ")
                     (if current-prefix-arg
                         (read-string "hg revision: ") ;; rev
                       nil)
                     nil ;; noupdate
                     nil ;; pull
                     ))
  (if rev
      (dvc-run-dvc-async 'xhg (list "clone" "--rev" rev src dest))
    (dvc-run-dvc-async 'xhg (list "clone" src dest))))

;;;###autoload
(defun xhg-dired-clone ()
  (interactive)
  (let* ((source (dired-filename-at-point))
         (target
          (read-string (format "Clone(%s)To: " (file-name-nondirectory source))
                       (file-name-directory source))))
    (xhg-clone source target)))

;;;###autoload
(defun xhg-incoming (&optional src show-patch no-merges)
  "Run hg incoming."
  (interactive (list (let* ((completions (xhg-paths 'both))
                            (initial-input (car (member "default" completions))))
                       (dvc-completing-read
                        "Show incoming from hg repository: "
                        completions nil nil initial-input))
                     nil ;; show-patch
                     nil ;; no-merges
                     ))
  (let ((window-conf (current-window-configuration))
        (buffer (dvc-get-buffer-create 'xhg 'log)))
    (dvc-switch-to-buffer-maybe buffer t)
    (let ((inhibit-read-only t))
      (erase-buffer))
    (xhg-log-mode)
    (dvc-run-dvc-async 'xhg (list "incoming" (when show-patch "--patch") (when no-merges "--no-merges") src)
                       :finished
                       (dvc-capturing-lambda (output error status arguments)
                         (progn
                           (with-current-buffer (capture buffer)
                             (let ((inhibit-read-only t))
                               (erase-buffer)
                               (insert-buffer-substring output)
                               (goto-char (point-min))
                               (insert (format "hg incoming for %s\n\n" default-directory))
                               (toggle-read-only 1)
                               (xhg-log-next 1)))))
                       :error
                       (dvc-capturing-lambda (output error status arguments)
                         (with-current-buffer output
                           (goto-char (point-max))
                           (forward-line -1)
                           (if (looking-at "no changes found")
                               (progn
                                 (message "No changes found")
                                 (set-window-configuration (capture window-conf)))
                             (dvc-default-error-function output error status arguments)))))))

;;;###autoload
(defun xhg-outgoing (&optional src show-patch no-merges)
  "Run hg outgoing."
  (interactive (list (let* ((completions (xhg-paths 'both))
                            (initial-input (car (member "default" completions))))
                       (dvc-completing-read
                        "Show outgoing to hg repository: "
                        completions nil nil initial-input))
                     nil ;; show-patch
                     nil ;; no-merges
                     ))
  (let ((window-conf (current-window-configuration))
        (buffer (dvc-get-buffer-create 'xhg 'log)))
    (dvc-switch-to-buffer-maybe buffer t)
    (let ((inhibit-read-only t))
      (erase-buffer))
    (xhg-log-mode)
    (dvc-run-dvc-async 'xhg (list "outgoing" (when show-patch "--patch") (when no-merges "--no-merges") src)
                       :finished
                       (dvc-capturing-lambda (output error status arguments)
                         (progn
                           (with-current-buffer (capture buffer)
                             (let ((inhibit-read-only t))
                               (erase-buffer)
                               (insert-buffer-substring output)
                               (goto-char (point-min))
                               (insert (format "hg outgoing for %s\n\n" default-directory))
                               (toggle-read-only 1)))))
                       :error
                       (dvc-capturing-lambda (output error status arguments)
                         (with-current-buffer output
                           (goto-char (point-max))
                           (forward-line -1)
                           (if (looking-at "no changes found")
                               (progn
                                 (message "No changes found")
                                 (set-window-configuration (capture window-conf)))
                             (dvc-default-error-function output error status arguments)))))))


(defun xhg-get-all-heads-list ()
  "Get a list of all heads available from the output of hg heads."
  (let ((rev-list (with-temp-buffer
                    (apply #'call-process "hg" nil t nil
                           '("heads"
                             "--template"
                             "#rev#\n"))
                    (buffer-string))))
    (setq rev-list (cons "auto"
                         (remove "" (split-string rev-list "\n"))))
    rev-list))

(defun xhg-changep ()
  (let ((change (with-temp-buffer
                  (apply #'call-process "hg" nil t nil
                         '("diff"))
                  (buffer-string))))
    (setq change (remove "" (split-string change "\n")))
    (if change
        t
      nil)))

;;;###autoload
(defun xhg-strip (rev)
  "Run hg strip."
  (interactive (list (dvc-completing-read "Remove head: "
                                          (xhg-get-all-heads-list))))
  (dvc-run-dvc-sync 'xhg (list "strip" rev)))

;;;###autoload
(defun xhg-merge ()
  "Run hg merge.
To merge from specific revision, choose it in completion with tab.
If `auto' is choose use default revision (last) unless there is ONLY
one more head.
See \(hg help merge.\)"
  (interactive)
  (let* ((haschange (xhg-changep))
         (collection (xhg-get-all-heads-list))
         (revision (dvc-completing-read "Merge from hg revision: "
                                        collection nil t))
         (arg))
    (when (or (string= revision "")
              (string= revision "auto"))
      (setq revision nil))
    (setq arg (if revision
                  '("merge" "--rev")
                  '("merge")))
    (cond ((and (> (length collection) 3)
                (not revision))
           (error "Abort: branch 'default' has more than 2 heads - please merge with an explicit rev."))
          ((equal revision (xhg-dry-tip))
           (error "Abort:can't merge with ancestor."))
          ((and (not haschange)
                (> (length collection) 2))
           (dvc-run-dvc-async 'xhg `(,@arg ,revision)
                              :finished
                              (dvc-capturing-lambda (output error status arguments)
                                (message "hg %s %s %s finished => %s"
                                         (nth 0 arg)
                                         (if revision
                                             (nth 1 arg)
                                             "")
                                         (if revision
                                             revision
                                             "")
                                         (concat (dvc-buffer-content error)
                                                 (dvc-buffer-content output))))
                              :error
                              ;; avoid dvc-error buffer to appear in ediff
                              (lambda (output error status arguments)
                                nil)))
          (haschange
           (error "abort: outstanding uncommitted merges, Please commit before merging"))
          ((< (length collection) 3)
           (error "There is nothing to merge here")))))

;;;###autoload
(defun xhg-resolve (&optional file)
  "Run hg resolve --all or <spec file>.
With current prefix arg, take a file as argument.
You should run xhg-merge before this.
This command will cleanly retry unresolved file merges
using file revisions preserved from the last update or merge.
If file is given resolve this file else resolve all files."
  (interactive)
  (let ((unresolved-files
         (loop for i in (xhg-resolve-list t)
              if (equal (car i) "U")
              collect (cadr i))))
    (when current-prefix-arg
      (setq file
            (file-name-nondirectory (read-file-name "File: "))))
    (if file
        (if (member file unresolved-files)
            (dvc-run-dvc-sync 'xhg (list "resolve" file)
                              :finished
                              (dvc-capturing-lambda (output error status arguments)
                                (message "ok finished with status %s" status)
                                (xhg-resolve-list)))
            (message "%s have been already resolved" file))
        (dvc-run-dvc-sync 'xhg (list "resolve" "--all")
                          :finished
                          (dvc-capturing-lambda (output error status arguments)
                            (message "ok finished with status %s" status)
                            (xhg-resolve-list))))))

;;;###autoload
(defun xhg-resolve-list (&optional quiet)
  "Run hg resolve --list.
Call interactively, show buffer with info.
Non interactively, return an alist with
string keys as:
U = unresolved
R = resolved"
  (interactive)
  (let ((resolve-alist nil))
    (if quiet
        (progn
          (save-window-excursion
            (dvc-run-dvc-display-as-info 'xhg (list "resolve" "--list"))
            (with-current-buffer "*xhg-info*"
              (setq resolve-alist
                    (mapcar #'split-string
                            (split-string (buffer-substring-no-properties
                                           (point-min)
                                           (point-max))
                                          "\n"))))
            (kill-buffer "*xhg-info*")
            resolve-alist))
        (dvc-run-dvc-display-as-info 'xhg (list "resolve" "--list")))))


(defun xhg-command-version ()
  "Run hg version."
  (interactive)
  (let ((version (dvc-run-dvc-sync 'xhg '("version")
                                   :finished 'dvc-output-buffer-handler)))
    (when (interactive-p)
      (message "Mercurial version: %s" version))
    version))

;;;###autoload
(defun xhg-branch (&optional new-name)
  "Run hg branch.
When called with a prefix argument, ask for the new branch-name, otherwise
display the current one."
  (interactive "P")
  (let ((branch (dvc-run-dvc-sync 'xhg (list "branch")
                                  :finished 'dvc-output-buffer-handler)))
    (if (not new-name)
        (progn
          (when (interactive-p)
            (message "xhg branch: %s" branch))
          branch)
        (when (interactive-p)
          (setq new-name (read-string (format "Change branch from '%s' to: " branch) nil nil branch)))
        (dvc-run-dvc-sync 'xhg (list "branch" new-name)))))

;;;###autoload
(defun xhg-branches (&optional only-list)
  "run xhg-branches"
  (interactive)
  (dvc-run-dvc-display-as-info 'xhg '("branches"))
  (let ((branchs-list (with-current-buffer "*xhg-info*"
                        (split-string (buffer-string) "\n"))))
    (when only-list
      (kill-buffer "*xhg-info*")
      (loop for i in branchs-list
         for e = (car (split-string i))
         when e
         collect e))))

(defun xhg-branches-sans-current ()
  "Run xhg-branches but remove current branch."
  (save-window-excursion
    (let ((cur-branch (xhg-branch))
          (branches (xhg-branches t)))
      (remove cur-branch branches))))

;;;###autoload
(defun xhg-merge-branch ()
  "Run hg merge <branch-name>.
Usually merge the change made in dev branch in default branch."
  (interactive)
  (let* ((current-branch (xhg-branch))
         (branch (dvc-completing-read "BranchName: "
                                      (xhg-branches-sans-current))))
    (when (y-or-n-p (format "Really merge %s in %s" branch current-branch))
      (dvc-run-dvc-sync 'xhg (list "merge" branch)
                        :finished
                        (dvc-capturing-lambda (output error status arguments)
                          (message "Updated! Don't forget to commit."))))))

;;todo: add support to specify a rev
(defun xhg-manifest ()
  "Run hg manifest."
  (interactive)
  (let ((buffer (dvc-get-buffer-create 'xhg 'manifest)))
    (dvc-run-dvc-sync 'xhg '("manifest")
                      :finished
                      (dvc-capturing-lambda (output error status arguments)
                        (progn
                          (with-current-buffer (capture buffer)
                            (let ((inhibit-read-only t))
                              (erase-buffer)
                              (insert-buffer-substring output)
                              (toggle-read-only 1)))
                          (dvc-switch-to-buffer (capture buffer)))))))

;;;###autoload
(defun xhg-tip ()
  "Run hg tip."
  (interactive)
  (dvc-run-dvc-display-as-info 'xhg '("tip")))

;;;###autoload
(defun xhg-heads ()
  "Run hg heads."
  (interactive)
  (dvc-run-dvc-display-as-info 'xhg '("heads")))

;;;###autoload
(defun xhg-parents ()
  "Run hg parents."
  (interactive)
  (dvc-run-dvc-display-as-info 'xhg '("parents")))

;;;###autoload
(defun xhg-identify ()
  "Run hg identify."
  (interactive)
  (let ((id))
    (dvc-run-dvc-sync 'xhg '("identify")
                      :finished
                      (lambda (output error status arguments)
                        (set-buffer output)
                        (goto-char (point-min))
                        (setq id
                              (buffer-substring-no-properties
                               (point)
                               (line-end-position))))
                      :error
                      (lambda (output error status arguments)
                        (setq id "<unknown>")))
    (when (interactive-p)
      (message "hg identity for %s: %s" default-directory id))
    id))

;;;###autoload
(defun xhg-verify ()
  "Run hg verify."
  (interactive)
  (dvc-run-dvc-display-as-info 'xhg '("verify")))

;;;###autoload
(defun xhg-showconfig ()
  "Run hg showconfig."
  (interactive)
  (dvc-run-dvc-display-as-info 'xhg '("showconfig")))

;;;###autoload
(defun xhg-paths (&optional type)
  "Run hg paths.
When called interactive, display them in an *xhg-info* buffer.
Otherwise the return value depends on TYPE:
'alias:    Return only alias names
'path:     Return only the paths
'both      Return the aliases and the paths in a flat list
otherwise: Return a list of two element sublists containing alias, path"
  (interactive)
  (if (interactive-p)
      (dvc-run-dvc-display-as-info 'xhg '("paths"))
    (let* ((path-list (dvc-run-dvc-sync 'xhg (list "paths")
                                        :finished 'dvc-output-buffer-split-handler))
           (lisp-path-list (mapcar '(lambda(arg) (dvc-split-string arg " = " arg)) path-list))
           (result-list))
      (cond ((eq type 'alias)
             (setq result-list (mapcar 'car lisp-path-list)))
            ((eq type 'path)
             (setq result-list (mapcar 'cadr lisp-path-list)))
            ((eq type 'both)
             (setq result-list (append (mapcar 'car lisp-path-list) (mapcar 'cadr lisp-path-list))))
            (t
             (setq result-list lisp-path-list))))))

;;;###autoload
(defun xhg-tag (rev name)
  "Run hg tag -r <REV> NAME."
  (interactive (list (read-from-minibuffer "Revision: "
                                           nil nil nil nil
                                           (xhg-dry-tip))
                     (read-string "TagName: ")))
  (dvc-run-dvc-sync 'xhg (list "tag" "-r" rev name)
                    :finished (lambda (output error status arguments)
                                (message "Ok revision %s tagged as %s"
                                         rev name))))

;;;###autoload
(defun xhg-tags ()
  "Run hg tags."
  (interactive)
  (dvc-run-dvc-display-as-info 'xhg '("tags")))

;; hg annotate: add support to edit the parameters
;; -r --rev        revision
;; -a --text       treat all files as text
;; -u --user       show user
;; -n --number     show revision number
;; -c --changeset  show changeset

;; (defun xhg-annotate ()
;;   "Run hg annotate."
;;   (interactive)
;;   (dvc-run-dvc-display-as-info 'xhg (append '("annotate") (dvc-current-file-list))))

;;;###autoload
(defun xhg-view ()
  "Run hg view."
  (interactive)
  (dvc-run-dvc-async 'xhg '("view")))

;;;###autoload
(defun xhg-export (rev fname)
  "Run hg export.
`xhg-export-git-style-patches' determines, if git style patches are created."
  (interactive (list (xhg-read-revision "Export revision: ")
                     (read-file-name "Export hg revision to: ")))
  (dvc-run-dvc-sync 'xhg (list "export" (when xhg-export-git-style-patches "--git") "-o" (expand-file-name fname) rev)
                    :finished
                    (lambda (output error status arguments)
                      (message "Exported revision %s to %s." rev fname))))

;;;###autoload
(defun xhg-import (patch-file-name &optional force)
  "Run hg import."
  (interactive (list (read-file-name "Import hg patch: " nil nil t (when (eq major-mode 'dired-mode)
                                                                     (file-name-nondirectory (dired-get-filename))))))
  (dvc-run-dvc-sync 'xhg (delete nil (list "import" (when force "--force") (expand-file-name patch-file-name)))
                    :finished
                    (lambda (output error status arguments)
                      (message "Imported hg patch from %s." patch-file-name))))

;;;###autoload
(defun xhg-undo ()
  "Run hg undo."
  (interactive)
  (let ((undo-possible (file-exists-p (concat (xhg-tree-root) ".hg/undo"))))
    (if undo-possible
        (save-window-excursion
          (xhg-log "-1" nil t)
          (if (yes-or-no-p "Undo this transaction? ")
              (progn
                (dvc-run-dvc-sync 'xhg (list "undo")
                                  :finished
                                  (lambda (output error status arguments)
                                    (message "Finished xhg undo."))))
            (message "xhg undo aborted.")))
      (message "xhg: No undo information available."))))

;;;###autoload
(defun xhg-update (&optional clean switch)
  "Run hg update.
When called with one prefix-arg run hg update -C (clean).
Called with two prefix-args run hg update -C <branch-name> (switch to branch)."
  (interactive)
  (let* ((opt-list (cond  ((or clean
                               (equal current-prefix-arg '(4)))
                           (list "update" "-C"))
                          ((or switch
                               (equal current-prefix-arg '(16)))
                           (list "update" "-C" (dvc-completing-read "BranchName: "
                                                                    (xhg-branches-sans-current))))
                          (t
                           (list "update"))))
         (opt-string (mapconcat 'identity opt-list " ")))
    (dvc-run-dvc-sync 'xhg opt-list
                      :finished
                      (lambda (output error status arguments)
                        (dvc-default-finish-function output error status arguments)
                        (message "hg %s complete for %s" opt-string default-directory)))))

(defun xhg-convert (source target)
  "Convert a foreign SCM repository to a Mercurial one.

   Accepted source formats [identifiers]:(Mercurial-1.1.2)
    - Mercurial [hg]
    - CVS [cvs]
    - Darcs [darcs]
    - git [git]
    - Subversion [svn]
    - Monotone [mtn]
    - GNU Arch [gnuarch]
    - Bazaar [bzr]

Be sure to add to your hgrc:
\[extensions\]
hgext.convert =

Read also: hg help convert.
"
  (interactive "DSource: \nsTarget: ")
  (message "Started hg conversion of [%s] to [%s] ..." source target)
  (dvc-run-dvc-async 'xhg (list "convert"
                                (expand-file-name source)
                                (expand-file-name target))
                     :finished (dvc-capturing-lambda (output error status arguments)
                                  (let ((default-directory (capture target)))
                                    (xhg-update))
                                  (message "hg: [%s] successfully converted to [%s]" (capture source) (capture target)))))

;; --------------------------------------------------------------------------------
;; hg serve functionality
;; --------------------------------------------------------------------------------

(defvar xhg-serve-parameter-list (make-hash-table :test 'equal)
  "A hash table that holds the mapping from work directory roots to
extra parameters used for hg serve.
The extra parameters are given as alist. The following example shows the supported settings:
'((port 8235) (name \"my-project\"))")

;;;###autoload
(defun xhg-serve-register-serve-parameter-list (working-copy-root parameter-list &optional start-server)
  "Register a mapping from a work directory root to a parameter list for hg serve.
When START-SERVER is given, start the server immediately.
Example usage:
 (xhg-serve-register-serve-parameter-list \"~/proj/simple-counter-1/\" '((port 8100) (name \"simple-counter\")))"
  (puthash (dvc-uniquify-file-name working-copy-root) parameter-list xhg-serve-parameter-list)
  (when start-server
    (let ((default-directory (dvc-uniquify-file-name working-copy-root)))
      (xhg-serve))))

(defun xhg-serve ()
  "Run hg serve --daemon.
See `xhg-serve-register-serve-parameter-list' to register specific parameters for the server process."
  (interactive)
  (let* ((tree-root (dvc-tree-root))
         (server-status-dir (concat tree-root ".xhg-serve/"))
         (parameter-alist (gethash (dvc-uniquify-file-name tree-root) xhg-serve-parameter-list))
         (port (or (cadr (assoc 'port parameter-alist)) 8000))
         (name (cadr (assoc 'name parameter-alist)))
         (errorlog (concat server-status-dir "error.log"))
         (accesslog (concat server-status-dir "access.log"))
         (pid-file (concat server-status-dir "server.pid")))
    (when (numberp port)
      (setq port (number-to-string port)))
    (unless (file-directory-p server-status-dir)
      (make-directory server-status-dir))
    (dvc-run-dvc-sync 'xhg (list "serve" "--daemon" (when port "--port") port (when name "--name") name
                                 "--pid-file" pid-file "--accesslog" accesslog "--errorlog" errorlog)
                      :finished (dvc-capturing-lambda (output error status arguments)
                                  (message "hg server started for %s, using port %s" tree-root port)))))

(defun xhg-serve-kill ()
  "Kill a hg serve process started with `xhg-serve'."
  (interactive)
  (let* ((tree-root (dvc-tree-root))
         (server-status-dir (concat tree-root ".xhg-serve/"))
         (pid-file (concat server-status-dir "server.pid"))
         (pid)
         (kill-status))
    (if (file-readable-p pid-file)
        (with-current-buffer
            (find-file-noselect pid-file)
          (setq pid (buffer-substring-no-properties (point-min) (- (point-max) 1)))
          (kill-buffer (current-buffer)))
      (message "no hg serve pid file found - aborting"))
    (when pid
      (setq kill-status (call-process "kill" nil nil nil pid))
      (if (eq kill-status 0)
          (progn
            (delete-file pid-file)
            (message "hg serve process killed."))
        (message "kill hg serve process failed, return status: %d" kill-status)))))

;; --------------------------------------------------------------------------------
;; dvc revision support
;; --------------------------------------------------------------------------------
;;;###autoload
(defun xhg-revision-get-last-revision (file last-revision)
  "Insert the content of FILE in LAST-REVISION, in current buffer.

LAST-REVISION looks like
\(\"path\" NUM)"
  (dvc-trace "xhg-revision-get-last-revision file:%S last-revision:%S" file last-revision)
  (let ((xhg-rev (int-to-string (nth 1 last-revision)))
        (default-directory (car last-revision)))
    ;; TODO: support the last-revision parameter??
    (insert (dvc-run-dvc-sync
             'xhg (list "cat" file)
             :finished 'dvc-output-buffer-handler-withnewline))))

;;;###autoload
(defun xhg-revision-get-last-or-num-revision (infile outfile &optional revision)
  "Run the command:
hg cat --rev <num revision> -o outputfile inputfile"
  (interactive
   (let* ((xhg-infile (read-file-name "InputFile: "))
          (xhg-outfile (read-file-name "OutputFile: "))
          (xhg-rev (if current-prefix-arg
                       (read-string "Revision: ")
                     "tip")))
     (setq xhg-infile (expand-file-name xhg-infile)
           xhg-outfile (concat (expand-file-name xhg-outfile)
                               "."
                               xhg-rev))
     (list xhg-infile xhg-outfile xhg-rev)))
  (dvc-run-dvc-sync 'xhg (list "cat"
                               "--rev"
                               revision
                               "-o"
                               outfile
                               infile)
                    :finished 'dvc-output-buffer-handler-withnewline)
  (message "%s extracted in %s at revision %s"
           (file-name-nondirectory infile)
           (file-relative-name outfile)
           revision))

;;;###autoload
(defun xhg-ediff-file-at-rev (file rev1 rev2 &optional keep-variants)
  "Ediff file at rev1 against rev2.
With prefix arg do not delete the files.
If rev1 or rev2 are empty, ediff current file against last revision.
Tip: to quit ediff, use C-u q to kill the ediffied buffers."
  (interactive (list (read-file-name "File:" nil (dvc-get-file-info-at-point))
                     (read-from-minibuffer "Rev1: " nil nil nil nil (xhg-dry-tip))
                     (read-string "Rev2: ")))
  (let* ((fname (expand-file-name file))
         (bfname (file-name-nondirectory file))
         (file1 (concat dvc-temp-directory "/" rev1 "-" bfname))
         (file2 (concat dvc-temp-directory "/" rev2 "-" bfname))
         (pref-arg (or keep-variants
                       current-prefix-arg)))
    (if (or (equal "" rev1)
            (equal "" rev2))
        (dvc-file-ediff fname)
        (unless (equal rev1 rev2)
          (xhg-revision-get-last-or-num-revision fname file1 rev1)
          (xhg-revision-get-last-or-num-revision fname file2 rev2)
          (ediff-files file1 file2)
          (unless pref-arg
            (delete-file file1)
            (delete-file file2))))))


;; --------------------------------------------------------------------------------
;; higher level commands
;; --------------------------------------------------------------------------------

(defvar xhg-submit-patch-mapping nil)
;;(add-to-list 'xhg-submit-patch-mapping '("~/data/wiki" ("joe@host.com" "my-wiki")))

(defun xhg-export-via-mail (rev)
  (interactive (list (xhg-read-revision "Export revision: ")))
  (let ((file-name)
        (destination-email "")
        (base-file-name nil)
        (subject)
        (description))
    (dolist (m xhg-submit-patch-mapping)
      (when (string= (dvc-uniquify-file-name (car m)) (dvc-uniquify-file-name (xhg-tree-root)))
        ;;(message "%S" (cadr m))
        (setq destination-email (car (cadr m)))
        (setq base-file-name (cadr (cadr m)))))
    (setq file-name (concat (dvc-uniquify-file-name dvc-temp-directory) (or base-file-name "") rev ".patch"))
    (xhg-export rev file-name)

    (setq description
          (dvc-run-dvc-sync 'xhg (list "log" "-r" rev)
                            :finished 'dvc-output-buffer-handler))

    (require 'reporter)
    (delete-other-windows)
    (reporter-submit-bug-report
     destination-email
     nil
     nil
     nil
     nil
     description)
    (save-excursion
      (re-search-backward "^summary: +\\(.+\\)")
      (setq subject (match-string-no-properties 1)))

    ;; delete emacs version - its not needed here
    (delete-region (point) (point-max))

    (mml-attach-file file-name "text/x-patch")
    (goto-char (point-min))
    (mail-position-on-field "Subject")
    (insert (concat "[PATCH] " subject))))

;; hg log -r $(hg identify)
;; add one to that revision number -> actual-rev+1
;; hg log -r actual-rev+1:tip, e.g. hg log -r 5:tip
;;;###autoload
(defun xhg-missing-1 ()
  "Shows the logs of the new arrived changesets after a pull and before an update."
  (interactive)
  (let ((id (split-string (xhg-identify)))
        (last-log)
        (actual-rev))
    (if (= 2 (length id))
        (message "Nothing missing, already at tip.")
      (if (string= (car id) "unknown")
          (setq actual-rev -1)
        (setq last-log (dvc-run-dvc-sync 'xhg (list "log" "-r" (car id))
                                         :finished 'dvc-output-buffer-handler))
        (string-match "changeset: +\\([0-9]+\\)" last-log)
        (setq actual-rev (string-to-number (match-string-no-properties 1 last-log))))
      (xhg-log (concat (number-to-string (+ actual-rev 1)) ":tip")))))

(defun xhg-save-diff (filename)
  "Save the current hg diff to a file named FILENAME."
  (interactive (list (read-file-name "Save the hg diff to: ")))
  (with-current-buffer
      (find-file-noselect filename)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (dvc-run-dvc-sync 'xhg (list "diff")
                                :finished 'dvc-output-buffer-handler-withnewline))
      (save-buffer)
      (kill-buffer (current-buffer)))))


;; --------------------------------------------------------------------------------
;; hgrc-mode
;; --------------------------------------------------------------------------------

(defun xhg-hgrc-open-hgrc-file (file-name)
  (find-file file-name)
  (unless (file-exists-p file-name)
    (insert "# -*- hgrc -*-\n\n")))

(defun xhg-hgrc-edit-repository-hgrc ()
  "Edit the .hg/hgrc file for the current working copy"
  (interactive)
  (xhg-hgrc-open-hgrc-file (concat (xhg-tree-root) ".hg/hgrc")))

(defun xhg-hgrc-edit-global-hgrc ()
  "Edit the ~/.hgrc file"
  (interactive)
  (xhg-hgrc-open-hgrc-file "~/.hgrc"))

;; Note: this mode is named hgrc-mode and not xhgrc-mode, because
;; a similar thing does not exist in mercurial.el yet and
;; that mode should be settable via a file local variable in .hgrc files

(defvar hgrc-mode-map
  (let ((map (make-sparse-keymap)))
    map)
  "Keymap used in `hgrc-mode'.")

(easy-menu-define hgrc-mode-menu hgrc-mode-map
  "`hgrc-mode' menu"
  `("hgrc"
    ["Show hgrc manpage" hgrc-mode-help t]
    ))

(dvc-do-in-gnu-emacs
  ;; TODO : define-generic-mode doesn't exist in XEmacs.
  ;; http://list-archive.xemacs.org/xemacs-beta/200408/msg00016.html
  ;; world be better to use define-derived-mode below
  (define-generic-mode 'hgrc-mode
    '(?\; ?#)
    nil
    '(("^\\(\\[.*\\]\\)" 1 font-lock-constant-face)
      ("^\\s-*\\(.+\\)=\\([^\r\n]*\\)"
       (1 font-lock-variable-name-face)
       (2 font-lock-type-face)))
    '("\\.?hgrc\\'")
    '(hgrc-mode-setup-function)
    "Mode to edit mercurial configuration files.")
  )

(dvc-do-in-xemacs
  (define-derived-mode hgrc-mode fundamental-mode
    "Hgrc-mode"
    "Major mode to edit hgrc files"
    ;; Empty mode for XEmacs users :-(
    ))

(defun hgrc-mode-setup-function ()
  (use-local-map hgrc-mode-map))

(defun hgrc-mode-help ()
  "Show the manual for the hgrc configuration file."
  (interactive)
  (split-window)
  (other-window 1)
  (apply (if (featurep 'xemacs) 'manual-entry 'woman) '("hgrc"))
  (other-window -1))

(provide 'xhg)
;;; xhg.el ends here
