;;; -*- Mode:Emacs-Lisp -*-
;;; This file contains font and menu hacks for BBDB.

;;; This file is the part of the Insidious Big Brother Database (aka BBDB),
;;; copyright (c) 1992, 1993, 1994 Jamie Zawinski <jwz@netscape.com>.

;;; The Insidious Big Brother Database is free software; you can redistribute
;;; it and/or modify it under the terms of the GNU General Public License as
;;; published by the Free Software Foundation; either version 2, or (at your
;;; option) any later version.
;;;
;;; BBDB is distributed in the hope that it will be useful, but WITHOUT ANY
;;; WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;;; details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; This code is kind of kludgey, mostly because it needs to parse the contents
;;; of the *BBDB* buffer, since BBDB doesn't save the buffer-positions of the
;;; various fields when it fills in that buffer (doing that would be slow and
;;; cons a lot, so it doesn't seem to be worth it.)

(require 'bbdb)
(require 'bbdb-com)

;; compiler whinage. Some of this is legacy stuff that would probably
;; be better deleted.
(defvar scrollbar-height nil)

;; MIGRATE XXX
(eval-and-compile
  (if (fboundp 'set-specifier)
      (fset 'bbdb-set-specifier 'set-specifier)
    (fset 'bbdb-set-specifier 'ignore))
  (if (fboundp 'make-glyph)
      (fset 'bbdb-make-glyph 'make-glyph)
    (fset 'bbdb-make-glyph 'ignore))
  (if (fboundp 'set-glyph-face)
      (fset 'bbdb-set-glyph-face 'set-glyph-face)
    (fset 'bbdb-set-glyph-face 'ignore))
  (if (fboundp 'highlight-headers-x-face)
      (fset 'bbdb-highlight-headers-x-face 'highlight-headers-x-face)
    (fset 'bbdb-highlight-headers-x-face 'ignore))
  (if (fboundp 'highlight-headers-x-face-to-pixmap)
      (fset 'bbdb-highlight-headers-x-face-to-pixmap 'highlight-headers-x-face-to-pixmap)
    (fset 'bbdb-highlight-headers-x-face-to-pixmap 'ignore)))


(if (string-match "XEmacs\\|Lucid" emacs-version)
    (progn
      (define-key bbdb-mode-map 'button3 'bbdb-menu)
      (define-key bbdb-mode-map 'button2 (lambda (e)
                                           (interactive "e")
                                           (mouse-set-point e)
                                           (bbdb-toggle-records-display-layout 0 ))))
  (define-key bbdb-mode-map [mouse-3] 'bbdb-menu)
  (define-key bbdb-mode-map [mouse-2] (lambda (e)
                                        (interactive "e")
                                        (mouse-set-point e)
                                        (bbdb-toggle-records-display-layout 0))))

(eval-and-compile
  (if (fboundp 'find-face)
      (fset 'bbdb-find-face 'find-face)
    (if (fboundp 'internal-find-face) ;; GRR.
        (fset 'bbdb-find-face 'internal-find-face)
      (fset 'bbdb-find-face 'ignore)))) ; noop - you probably don't HAVE faces.

(or (bbdb-find-face 'bbdb-name)
    (face-differs-from-default-p (make-face 'bbdb-name))
    (set-face-underline-p 'bbdb-name t))

(condition-case data
    (or (bbdb-find-face 'bbdb-company)
        (face-differs-from-default-p (make-face 'bbdb-company))
        (make-face-italic 'bbdb-company)) ;; this can fail on emacs
  (error nil))

(or (bbdb-find-face 'bbdb-field-value)
    (make-face 'bbdb-field-value))

(or (bbdb-find-face 'bbdb-field-name)
    (face-differs-from-default-p (make-face 'bbdb-field-name))
    (copy-face 'bold 'bbdb-field-name))

;;; Extents vs. Overlays unhappiness
;;; FIXME: see if VM is around, and call its extents code instead;
;;; change bbdb-foo-extents below to vm-foo-extents, etc.
(eval-and-compile
  (if (fboundp 'make-extent)
      (fset 'bbdb-make-extent 'make-extent)
    (fset 'bbdb-make-extent 'make-overlay))

  (if (fboundp 'delete-extent)
      (fset 'bbdb-delete-extent 'delete-extent)
    (fset 'bbdb-delete-extent 'delete-overlay))

  (if (fboundp 'mapcar-extents)
      (defmacro bbdb-list-extents() `(mapcar-extents 'identity))
    (defun bbdb-list-extents()
      (let ((o (overlay-lists))) (nconc (car o) (cdr o)))))

  (if (fboundp 'set-extent-property)
      (fset 'bbdb-set-extent-property 'set-extent-property)
    (defun bbdb-set-extent-property( e p v )
      (if (eq 'highlight p)
          (if v
              (overlay-put e 'mouse-face 'highlight)
            (overlay-put e 'mouse-face nil)))
      (overlay-put e p v)))

  (if (fboundp 'extent-property)
      (fset 'bbdb-extent-property 'extent-property)
    (fset 'bbdb-extent-property 'overlay-get))

  (if (fboundp 'extent-at)
      (fset 'bbdb-extent-at 'extent-at)
    (defun bbdb-extent-at (pos buf tag) "NOT FULL XEMACS IMPLEMENTATION"
      (let ((o (overlays-at pos))
            minpri retval)
        (while (car o)
          (let ((x (car o)))
            (and (overlayp x)
                 (overlay-get x tag)
                 (if (or (null minpri) (> minpri (overlay-get x 'priority)))
                     (setq retval x
                           minpri (overlay-get x 'priority))))
            (setq o (cdr o))))
        retval)))

  (if (fboundp 'highlight-extent)
      (fset 'bbdb-highlight-extent 'highlight-extent)
    (fset 'bbdb-highlight-extent 'ignore)) ; XXX noop

  (if (fboundp 'extent-start-position)
      (fset 'bbdb-extent-start-position 'extent-start-position)
    (fset 'bbdb-extent-start-position 'overlay-start))

  (if (fboundp 'extent-end-position)
      (fset 'bbdb-extent-end-position 'extent-end-position)
    (fset 'bbdb-extent-end-position 'overlay-end))

  (if (fboundp 'extent-face)
      (fset 'bbdb-extent-face 'extent-face)
    (defun bbdb-extent-face (extent)
      (overlay-get extent 'face)))

  (if (fboundp 'set-extent-face)
      (fset 'bbdb-set-extent-face 'set-extent-face)
    (defun bbdb-set-extent-face (extent face) "set the face for an overlay"
      (overlay-put extent 'face face)))

  (if (fboundp 'set-extent-begin-glyph)
      (fset 'bbdb-set-extent-begin-glyph 'set-extent-begin-glyph)
    (fset 'bbdb-set-extent-begin-glyph 'ignore)) ; XXX noop

  (if (fboundp 'set-extent-end-glyph)
      (fset 'bbdb-set-extent-end-glyph 'set-extent-end-glyph)
    (fset 'bbdb-set-extent-end-glyph 'ignore))) ; XXX noop


;;;###autoload
(defun bbdb-fontify-buffer ()
  (save-excursion
    (set-buffer bbdb-buffer-name)
    (if (featurep 'scrollbar)
        (bbdb-set-specifier scrollbar-height (cons (current-buffer) 0)))
    ;; first delete existing extents
    (mapcar (function (lambda(o)
                        (if o  ;; may start with nil
                            (if (eq (bbdb-extent-property o 'data) 'bbdb)
                                (bbdb-delete-extent o)))))
            (bbdb-list-extents))
    (let ((rest bbdb-records)
          record face start end elided-p p e)
      (while rest
        (setq record (car (car rest))
              elided-p (eq (nth 1 (car rest)) t)
              face (and (not elided-p) (bbdb-record-getprop record 'face))
              start (marker-position (nth 2 (car rest)))
              end (1- (or (nth 2 (car (cdr rest))) (point-max))))
        (bbdb-set-extent-property (setq e (bbdb-make-extent start end))
                                  'highlight t)
        (bbdb-set-extent-property e 'data 'bbdb)
        ;; note that on GNU Emacs, once you hit the main overlay, you
        ;; have to move off the record and back on again before it'll
        ;; notice that you're on a more specific overlay. This is
        ;; bogus, like most GNU Emacs GUI stuff.
        (bbdb-set-extent-property e 'priority 3)
        (setq p (+ start (length (bbdb-record-name record))))
        (if (bbdb-record-company record)
          (setq p (next-single-property-change (+ p 3) 'bbdb-field)))
        (goto-char start)
        (if (search-forward " - " p t)
            (progn
              (setq e (bbdb-make-extent (point) p))
              (bbdb-set-extent-property e 'data 'bbdb)
              (bbdb-set-extent-face e 'bbdb-company)
              (bbdb-set-extent-property e 'highlight t)
              (bbdb-set-extent-property e 'priority 2)
              (forward-char -3))
          (goto-char p))
        (setq e (bbdb-make-extent start (point)))
        (bbdb-set-extent-property e 'data 'bbdb)
        (bbdb-set-extent-face e 'bbdb-name)
        (bbdb-set-extent-property e 'priority 2)
        (bbdb-set-extent-property e 'highlight t)
        (if face (bbdb-hack-x-face face e))
        (forward-line 1)
        (while (< (point) end)
          (skip-chars-forward " \t")
          (setq p (point))
          (and (looking-at "[^:\n]+:")
               (progn
                 (setq e (bbdb-make-extent p (match-end 0)))
                 (bbdb-set-extent-face e 'bbdb-field-name)
                 (bbdb-set-extent-property e 'priority 2)
                 (bbdb-set-extent-property e 'data 'bbdb)))
          (while (progn (forward-line 1)
                        (looking-at "^\\(\t\t \\|                 \\)")))
          (setq e (bbdb-make-extent p (1- (point))))
          (bbdb-set-extent-property e 'data 'bbdb)
          (bbdb-set-extent-face e 'bbdb-field-value)
          (bbdb-set-extent-property e 'priority 2)
          (bbdb-set-extent-property e 'highlight t))
        (setq rest (cdr rest))))))

;;; share the xface cache data with VM if it's around
(defvar vm-xface-cache (make-vector 29 0))

(defun bbdb-hack-x-face (face extent)
  "Process a face property of a record and honour it.
Not done for GNU Emacs just yet, since it doesn't have image support
as of GNU Emacs 20.7"
  (if (not (or (and (boundp 'highlight-headers-hack-x-face-p)
                    (funcall (intern                               ;; compiler
                              "highlight-headers-hack-x-face-p"))) ;; ick.
               (and (featurep 'xemacs)
                    (string-match "^21\\." emacs-version)))) ;; XXX
      () ;; nothing doing
    (setq face (bbdb-split face "\n"))
    (while face
      (cond

       ;; ripped pretty much verbatim from VM; X Faces for recent XEmacsen.
       ((string-match "^21\\." emacs-version) ;; XXX how far back can I go?
        (condition-case data
            (let* ((h (concat "X-Face: " (car face))) ;; from vm-display-xface
                   (g (intern h vm-xface-cache)))
              (if (bbdb-find-face 'vm-xface) ;; use the same face as VM
                  nil
                (make-face 'vm-xface)
                (set-face-background 'vm-xface "white")
                (set-face-foreground 'vm-xface "black"))
              (if (boundp g)
                  (setq g (symbol-value g))
                (set g (bbdb-make-glyph
                        (list
                         (vector 'xface ':data h)))) ;; XXX use API
                (setq g (symbol-value g))
                (bbdb-set-glyph-face g 'vm-xface))
              (bbdb-set-extent-property extent 'vm-xface t)
              (bbdb-set-extent-begin-glyph extent g))
          (error nil))) ;; looks like you don't have xface support, d00d

       ;; requires lemacs 19.10 version of highlight-headers.el
       ((fboundp 'highlight-headers-x-face)                     ; the 19.10 way
        (bbdb-highlight-headers-x-face (car face) extent)
        (let ((b (bbdb-extent-property extent 'begin-glyph)))
          (cond (b ; I'd like this to be an end-glyph instead
                 (bbdb-set-extent-property extent 'begin-glyph nil)
                 (bbdb-set-extent-property extent 'end-glyph b)))))

       ((fboundp 'highlight-headers-x-face-to-pixmap)           ; the 19.13 way
        (save-excursion
          (set-buffer (get-buffer-create " *tmp*"))
          (buffer-disable-undo (current-buffer))
          (erase-buffer)
          (insert (car face))
          (bbdb-set-extent-begin-glyph extent nil)
          (bbdb-set-extent-end-glyph extent
                                (bbdb-highlight-headers-x-face-to-pixmap
                                 (point-min) (point-max)))
          (erase-buffer))))

      ;; more faces?
      (setq face (cdr face))
      (cond (face ; there are more, so clone the extent
             (setq extent (bbdb-make-extent
                           (bbdb-extent-start-position extent)
                           (bbdb-extent-end-position extent)))
             (bbdb-set-extent-property extent 'data 'bbdb))))))


(defvar bbdb-user-menu-commands nil
  "User defined menu entries which should be appended to the BBDB menu." )

(defun build-bbdb-finger-menu (record)
  (let ((addrs (bbdb-record-finger-host record)))
    (if (cdr addrs)
        (cons "Finger..."
              (nconc
               (mapcar (lambda (addr)
                         (vector addr (list 'bbdb-finger record addr)
                                 t))
                       addrs)
               (list "----"
                     (vector "Finger all addresses"
                             (list 'bbdb-finger record ''(4)) t))))
      (vector (concat "Finger " (car addrs))
              (list 'bbdb-finger record (car addrs)) t))))

(defun build-bbdb-sendmail-menu (record)
  (let ((addrs (bbdb-record-net record)))
    (if (cdr addrs)
        (cons "Send Mail..."
              (mapcar (lambda (addr)
                        (vector addr (list 'bbdb-send-mail-internal
                                           (bbdb-dwim-net-address record addr))
                                t))
                      addrs))
      (vector (concat "Send mail to " (car addrs))
              (list 'bbdb-send-mail-internal
                    (bbdb-dwim-net-address record (car addrs)))
              t))))


(defun build-bbdb-field-menu (record field)
  (let ((type (car field)))
    (nconc
     (list
      (concat "Commands for "
              (cond ((eq type 'property)
                     (concat "\""
                             (symbol-name (if (consp (car (cdr field)))
                                              (car (car (cdr field)))
                                            (car (cdr field))))
                             "\" field:"))
                    ((eq type 'name) "Name field:")
                    ((eq type 'company) "Company field:")
                    ((eq type 'net) "Network Addresses field:")
                    ((eq type 'aka) "Alternate Names field:")
                    (t
                     (concat "\"" (aref (nth 1 field) 0) "\" "
                             (capitalize (symbol-name type)) " field:"))))
      "-----"
      ["Edit Field" bbdb-edit-current-field t]
      )
     (if (memq type '(name company))
         nil
       (list ["Delete Field" bbdb-delete-current-field-or-record t]))
     (cond ((eq type 'phone)
            (list (vector (concat "Dial " (bbdb-phone-string (car (cdr field))))
                          (list 'bbdb-dial (list 'quote field) nil) t)))
           )
     )))


(defun build-bbdb-insert-field-menu (record)
  (cons "Insert New Field..."
        (mapcar
         (lambda (field)
           (let ((type (if (string= (car field) "AKA")
                           'aka
                         (intern (car field)))))
             (vector (car field)
                     (list 'bbdb-insert-new-field (list 'quote type)
                           (list 'bbdb-prompt-for-new-field-value
                                 (list 'quote type)))
                     (not
                      (or (and (eq type 'net) (bbdb-record-net record))
                          (and (eq type 'aka) (bbdb-record-aka record))
                          (and (eq type 'notes) (bbdb-record-notes record))
                          (and (consp (bbdb-record-raw-notes record))
                               (assq type (bbdb-record-raw-notes record))))))))
         (append '(("phone") ("address") ("net") ("AKA") ("notes"))
                 (bbdb-propnames)))))


(defun build-bbdb-menu (record field)
  (delete
   nil
   (append
    '("bbdb-menu" "Global BBDB Commands" "-----")
    (list
     ["Save BBDB" bbdb-save-db t]
     ["Toggle All Records Display Layout"
      bbdb-toggle-all-records-display-layout t]
    ["Finger All Records" (bbdb-finger (mapcar 'car bbdb-records)) t]
    ["BBDB Manual" bbdb-info t]
    ["BBDB Quit" bbdb-bury-buffer t])
    (if record
        (list
         "-----"
         (concat "Commands for record \""
                 (bbdb-record-name record) "\":")
         "-----"
         (vector "Delete Record"
                 (list 'bbdb-delete-current-record record) t)
         ["Toggle Records Display Layout" bbdb-toggle-records-display-layout t]
         (if (and (not (eq 'full-multi-line
                           (nth 1 (assq record bbdb-records))))
                  (bbdb-display-layout-get-option 'multi-line 'omit))
             ["Fully Display Record" bbdb-display-record-completely t])
         ["Omit Record" bbdb-omit-record t]
         ["Refile (Merge) Record" bbdb-refile-record t]
         ))
    (if record
        (list (build-bbdb-finger-menu record)))
    (if (bbdb-record-net record)
        (list (build-bbdb-sendmail-menu record)))
    (if record
        (list (build-bbdb-insert-field-menu record)))
    (if field
        (cons "-----" (build-bbdb-field-menu record field)))
    bbdb-user-menu-commands)))


(eval-and-compile
  (if (fboundp 'popup-menu)
      (fset 'bbdb-popup 'popup-menu)
    ;; This is really, REALLY ugly, but it saves me some coding and uses
    ;; the correct keymap API instead of carnal knowledge of keymap
    ;; structure.
    (defun bbdb-desc-to-menu(desc)
      (let ((map (make-sparse-keymap (car desc)))
            (desc (reverse (cdr desc))) ;; throw away header, reorient list
            (txtcount 0) elt elt-name)
        (while (setq elt (car desc))
          ;; fake a key binding name
          (setq elt-name (intern (format "fake%d" txtcount))
                txtcount (+ 1 txtcount))
          (cond
           ;; non-active entries in the menu
           ((stringp elt)
            (define-key map (vector elt-name) (list elt)))

           ;; active entries in the menu
           ((vectorp elt)
            (define-key map (vector elt-name) (cons (aref elt 0) (aref elt 1))))

           ;; submenus
           ((listp elt)
            (define-key map (vector elt-name)
              (cons (car elt) (bbdb-desc-to-menu elt))))
           )
          (setq desc (cdr desc)))
        map))
    ;; this does the actual popping up & parsing nonsense
    (defun bbdb-popup( desc &optional event )
      (let ((map (bbdb-desc-to-menu desc)) result)
        (setq result (x-popup-menu t map))
        (if result
            (let ((command (lookup-key map (vconcat result))))
              ;; Clear out echoing, which perhaps shows a prefix arg.
              (message "")
              (if command
                  (if (commandp command)
                      (command-execute command)
                    (funcall 'eval command)))))))))

;;;###autoload
(defun bbdb-menu (e)
  (interactive "e")
  (mouse-set-point e)
  (bbdb-popup
   (save-window-excursion
     (save-excursion
       (let ((extent (or (bbdb-extent-at (point) (current-buffer) 'highlight)
                         (error "")))
             record field face)
         (or (eq (bbdb-extent-property extent 'data) 'bbdb)
             (error "not a bbdb extent"))
         (bbdb-highlight-extent extent t)
         (setq record (bbdb-current-record)
               field  (get-text-property (point) 'bbdb-field))
         (build-bbdb-menu record field))))))

;; tell everyone else we're here.
(provide 'bbdb-gui)
