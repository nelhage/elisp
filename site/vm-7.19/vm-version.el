;;(provide 'vm)
;;(provide 'vm-version)

(defconst vm-version "7.19"
  "Version number of VM.")

(defun vm-version ()
  "Returns the value of the variable vm-version."
  vm-version)

(defconst vm-xemacs-p nil)
(defconst vm-xemacs-mule-p nil)
(defconst vm-xemacs-file-coding-p nil)
(defconst vm-fsfemacs-p nil)
(defconst vm-fsfemacs-mule-p nil)
(defun vm-xemacs-p () vm-xemacs-p)
(defun vm-xemacs-mule-p () vm-xemacs-mule-p)
(defun vm-xemacs-file-coding-p () vm-xemacs-file-coding-p)
(defun vm-fsfemacs-p () vm-fsfemacs-p)
(defun vm-fsfemacs-mule-p () vm-fsfemacs-mule-p)
(defun vm-note-emacs-version ()
  (setq vm-xemacs-p (string-match "XEmacs" emacs-version)
	vm-xemacs-mule-p (and vm-xemacs-p (featurep 'mule)
			      ;; paranoia
			      (fboundp 'set-buffer-file-coding-system))
	vm-xemacs-file-coding-p (and vm-xemacs-p (featurep 'file-coding)
				     ;; paranoia
				     (fboundp 'set-buffer-file-coding-system))
	vm-fsfemacs-p (not vm-xemacs-p)
	vm-fsfemacs-mule-p (and (not vm-xemacs-mule-p) (featurep 'mule)
				(fboundp 'set-buffer-file-coding-system))))
(vm-note-emacs-version)

(defun vm-mouse-fsfemacs-mouse-p ()
  (and vm-fsfemacs-p
       (fboundp 'set-mouse-position)))

(defun vm-mouse-xemacs-mouse-p ()
  (and vm-xemacs-p
       (fboundp 'set-mouse-position)))

(defun vm-menu-fsfemacs-menus-p ()
  (and vm-fsfemacs-p
       (fboundp 'menu-bar-mode)))

(defun vm-menu-fsfemacs19-menus-p ()
  (and vm-fsfemacs-p
       (fboundp 'menu-bar-mode)
       (= emacs-major-version 19)))

(defun vm-menu-xemacs-menus-p ()
  (and vm-xemacs-p
       (fboundp 'set-buffer-menubar)))

(defun vm-menu-can-eval-item-name ()
  (and vm-xemacs-p
       (fboundp 'check-menu-syntax)
       (condition-case nil
	   (check-menu-syntax '("bar" ((identity "foo") 'ding t)))
	 (error nil))))

(defun vm-multiple-frames-possible-p () 
  (cond (vm-xemacs-p 
	 (or (memq 'win (device-matching-specifier-tag-list))
	     (featurep 'tty-frames)))
        (vm-fsfemacs-p 
         (fboundp 'make-frame))))
 
(defun vm-mouse-support-possible-p () 
  (cond (vm-xemacs-p 
         (featurep 'window-system)) 
        (vm-fsfemacs-p 
         (fboundp 'track-mouse))))
 
(defun vm-mouse-support-possible-here-p ()
  (cond (vm-xemacs-p
	 (memq 'win (device-matching-specifier-tag-list)))
	(vm-fsfemacs-p
	 (memq window-system '(x mac w32 win32)))))

(defun vm-menu-support-possible-p ()
  (cond (vm-xemacs-p
	 (featurep 'menubar))
	(vm-fsfemacs-p
	 (fboundp 'menu-bar-mode))))
 
(defun vm-toolbar-support-possible-p ()
  (or (and vm-xemacs-p (featurep 'toolbar))
      (and vm-fsfemacs-p (fboundp 'tool-bar-mode) (boundp 'tool-bar-map))))

(defun vm-multiple-fonts-possible-p ()
  (cond (vm-xemacs-p
	 (memq (device-type) '(x gtk mswindows)))
	(vm-fsfemacs-p
	 (memq window-system '(x mac w32 win32)))))

(defun vm-images-possible-here-p ()
  (or (and vm-xemacs-p (memq (device-type) '(x gtk mswindows)))
      (and vm-fsfemacs-p window-system
	   (or (fboundp 'image-type-available-p)
	       (and (stringp vm-imagemagick-convert-program)
		    (stringp vm-imagemagick-identify-program))))))

(defun vm-image-type-available-p (type)
  (if (fboundp 'image-type-available-p)
      (image-type-available-p type)
    (or (featurep type) (eq type 'xbm))))

(provide 'vm)
(provide 'vm-version)
