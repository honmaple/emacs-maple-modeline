;;; maple-modeline.el --- modeline configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2015-2019 lin.jiang

;; Author: lin.jiang <mail@honmaple.com>
;; Version: 0.1.0
;; Package-Requires: (maple-xpm)
;; URL: https://github.com/honmaple/emacs-maple-modeline
;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; modeline configurations.
;;

;;; Code:
(require 'all-the-icons nil t)
(require 'subr-x)
(require 'maple-modeline-window)
(require 'maple-xpm)

(defvar pyvenv-virtual-env-name)
(defvar maple-modeline--format nil)
(defvar maple-modeline--message nil)
(defvar maple-modeline--message-timer nil)
(defvar maple-modeline--message-active nil)

(defgroup maple-modeline nil
  "Maple-modeline, a prettier mode line."
  :group 'maple)

(defcustom maple-modeline-style 'standard
  "Maple-modeline Style."
  :group 'maple-modeline
  :type '(choice (const standard)
                 (const minimal)
                 (const sidebar)))

(defcustom maple-modeline-side-style 'sidebar
  "Maple-modeline auto set side window style."
  :group 'maple-modeline
  :type '(choice (const standard)
                 (const minimal)
                 (const sidebar)))

(defcustom maple-modeline-width 'auto
  "Maple-modeline witdh."
  :group 'maple-modeline
  :type '(choice (const standard)
                 (const auto)))

(defcustom maple-modeline-sep 'maple-xpm-draw
  "Maple-modeline draw separator function."
  :group 'maple-modeline
  :type 'function)

(defcustom maple-modeline-icon (display-graphic-p)
  "Maple-modeline whether show icon."
  :group 'maple-modeline
  :type 'boolean)

(defcustom maple-modeline-background
  (if (display-graphic-p) "#35331D" "#333333")
  "Maple-modeline background color."
  :group 'maple-modeline
  :type 'string)

(defcustom maple-modeline-direction
  (if (display-graphic-p) '(auto . auto) '(right . left))
  "Maple-modeline show direction."
  :group 'maple-modeline
  :type 'cons)

(defcustom maple-modeline-face
  '((window-number . maple-modeline-evil-face)
    (remote-host . mode-line-buffer-id)
    (projectile . mode-line-buffer-id)
    (process . maple-modeline-active2)
    (python . maple-modeline-active2)
    (region . maple-modeline-active2)
    (macro . mode-line-buffer-id)
    (iedit . mode-line-buffer-id))
  "Maple-modeline face define."
  :group 'maple-modeline
  :type '(cons))

(defcustom maple-modeline-face-inherit t
  "Maple-modeline background inherit."
  :group 'maple-modeline
  :type 'boolean)

(defcustom maple-modeline-message-interval 3
  "Maple-modeline flush message time."
  :group 'maple-modeline
  :type 'integer)

(defcustom maple-modeline-priority
  '((lsp . 1)
    (projectile . 2)
    (remote-host . 3)
    (anzu . 5)
    (iedit . 5)
    (marco . 6)
    (misc-info . 6)
    (python . 6)
    (region . 6)
    (process . 7)
    (minor-mode . 7)
    (version-control . 7)
    (count . 8)
    (flycheck . 8)
    (position . 9)
    (major-mode . 9)
    (buffer-info . 9)
    (message . 10)
    (window-number . 10)
    (bar . 10))
  "Maple-modeline priority define."
  :group 'maple-modeline
  :type '(cons))

(defface maple-modeline-active0
  '((t (:inherit mode-line)))
  "Maple-modeline active face 0."
  :group 'maple-modeline)

(defface maple-modeline-active1
  `((t (:inherit mode-line :background ,maple-modeline-background)))
  "Maple-modeline active face 1."
  :group 'maple-modeline)

(defface maple-modeline-active2
  '((t (:foreground "plum1" :distant-foreground "DarkMagenta" :weight bold)))
  "Face for highlighting the python."
  :group 'maple-modeline)

(defface maple-modeline-active3
  '((t (:foreground "#AE81FF" :weight bold)))
  "Face for anzu."
  :group 'maple-modeline)

(defface maple-modeline-inactive0
  '((t (:inherit mode-line-inactive)))
  "Maple-modeline inactive face 0."
  :group 'maple-modeline)

(defface maple-modeline-inactive1
  `((t (:inherit mode-line-inactive :background ,maple-modeline-background)))
  "Maple-modeline inactive face 1."
  :group 'maple-modeline)

(defun maple-modeline-highlight-face(face)
  "Get highlight face from cursor with default FACE."
  (let ((background (face-attribute 'cursor :background))
        (foreground (face-attribute face :background nil t)))
    `(:background ,background :foreground ,foreground)))

(defun maple-modeline-evil-face(face)
  "Get evil face from cursor with default FACE."
  (let ((foreground (face-attribute 'cursor :background))
        (background (face-attribute face :background nil t)))
    `(:foreground ,foreground :background ,background)))

(defun maple-modeline-message-around(func &rest args)
  "Disable message display with echoarea FUNC ARGS."
  (unless inhibit-message
    (let* ((inhibit-message t)
           (msg (apply func args)))
      (unless (maple-modeline--nil-p msg)
        (setq maple-modeline--message msg)
        (force-mode-line-update)
        ;; auto flush message
        (when (timerp maple-modeline--message-timer)
          (cancel-timer maple-modeline--message-timer))
        (when (> maple-modeline-message-interval 0)
          (setq maple-modeline--message-timer
                (run-with-idle-timer maple-modeline-message-interval nil 'maple-modeline-message-flush)))))))

(defun maple-modeline-message-flush()
  "Flush message variable."
  (when maple-modeline--message
    (setq maple-modeline--message nil)
    (force-mode-line-update)))

(defun maple-modeline-render(s face0 face1 &optional sep direction start-direction prepend append1)
  "S FACE0 FACE1 &OPTIONAL SEP DIRECTION START-DIRECTION PREPEND APPEND1."
  (let* ((reverse (not (or (eq direction 'right)
                           (and (eq direction 'auto)
                                (eq start-direction 'right)))))
         (face nil)
         (r (cl-loop for z in s append
                     (let* ((face2 (maple-modeline--face z face1))
                            (str (maple-modeline-raw z face2))
                            (typ (if (symbolp z) (maple-modeline--nil-p str t) (string= str "")))
                            (sp (or sep (funcall maple-modeline-sep (or face face0) face2 reverse))))
                       (unless typ
                         (when (eq direction 'auto)
                           (setq reverse (not reverse)))
                         (setq face  face2)
                         (setq face0 (prog1 face1 (setq face1 face0)))
                         (list sp str))))))
    (append (if prepend r (cdr r))
            (when append1 (list (or sep (funcall maple-modeline-sep (or face face0) face1 reverse)))))))

(defun maple-modeline-display(l r &optional sep)
  "L R &OPTIONAL SEP PREPEND APPEND."
  (let* ((active (maple-modeline--active))
         (face0  (if active 'maple-modeline-active0 'maple-modeline-inactive0))
         (face1  (if active 'maple-modeline-active1 'maple-modeline-inactive1))
         (ld (car maple-modeline-direction))
         (rd (cdr maple-modeline-direction))
         (lv (maple-modeline-render l face0 face1 sep ld nil nil t))
         (rv (maple-modeline-render r face0 face1 sep rd nil t nil))
         (cv (maple-modeline-fill (string-width (format-mode-line rv))))
         (rrv (if (cl-evenp (/ (length lv) 2))
                  (maple-modeline-render (append (list cv) r) face0 face1 sep rd)
                (maple-modeline-render (append (list cv) r) face1 face0 sep rd 'right))))
    (cond ((not (car r)) (string-join lv))
          ((not (car l)) (string-join (append (list cv) rv)))
          (t (string-join (append lv rrv))))))

(defun maple-modeline--property-substrings (str prop)
  "Return a list of substrings of STR when PROP change."
  (let ((beg 0) (end 0)
        (len (length str))
        (out))
    (while (< end (length str))
      (setq end (or (next-single-property-change beg prop str) len))
      (setq out (append out (list (substring str beg (setq beg end))))))
    out))

(defun maple-modeline--add-text-property (str prop val)
  "Return proerty str of STR when PROP VAL."
  (mapconcat
   (lambda (mm)
     (let ((cur (get-text-property 0 'face mm)))
       (propertize mm 'face (append (if (listp cur) cur (list cur)) (list val)))))
   (maple-modeline--property-substrings str prop) ""))

(defun maple-modeline--face(item &optional default)
  "Get ITEM's face, when nil use DEFAULT."
  (let* ((name (if (listp item) (car item) item))
         (face (cdr (assq name maple-modeline-face))))
    (cond ((and face (facep face))
           (append
            (list :inherit face)
            (when maple-modeline-face-inherit (list :background (face-attribute default :background nil t)))))
          ((and face (listp face))
           (append
            face
            (when maple-modeline-face-inherit (list :background (face-attribute default :background nil t)))))
          (face (funcall face default))
          (t default))))

(defun maple-modeline--nil-p(str &optional trim)
  "Check STR TRIM is empty."
  (cond ((stringp str)
         (string= (if trim (string-trim str) str) ""))
        ((not str) t)))

(defun maple-modeline-icon(name face &rest args)
  "Draw icon with NAME and FACE ARGS."
  (apply 'all-the-icons-octicon
         (append (list name) args (list :v-adjust -0.05 :face face))))

(defun maple-modeline-raw(str &optional face left right)
  "Render1 STR as mode-line data with FACE LEFT RIGHT."
  (let ((left (cond ((not left) " ")
                    ((stringp left) left)
                    (t (maple-modeline-raw left face))))
        (right (cond ((not right) " ")
                     ((stringp right) right)
                     (t (maple-modeline-raw right face)))))
    (cond ((maple-modeline--nil-p str) "")
          ((symbolp str)
           (funcall (intern (format "maple-modeline-%s" str)) face left right))
          ((listp str)
           (let ((args (cdr str)))
             (maple-modeline-raw (car str) face (plist-get args :left) (plist-get args :right))))
          (t (let ((padded-str (concat left str right)))
               (if face (maple-modeline--add-text-property padded-str 'face face) padded-str))))))

(defun maple-modeline-fill (reserve)
  "Return empty space leaving RESERVE space on the right."
  (unless reserve (setq reserve 20))
  (when (and window-system (eq 'right (get-scroll-bar-mode)))
    (setq reserve (- reserve 3)))
  (propertize " " 'display `((space :align-to (- right ,reserve)))))

(defmacro maple-modeline-set (name &rest args)
  "Set modeline with NAME and ARGS."
  (declare (indent 1) (doc-string 2))
  (let* ((-name (format "%s" name))
         (-left (or (plist-get args :left) '()))
         (-right (or (plist-get args :right) '()))
         (-sep (plist-get args :sep)))
    `(defun ,(intern (format "maple-modeline-format-%s" -name)) ()
       (maple-modeline-display ,-left ,-right ,-sep))))

(defmacro maple-modeline-define (name &rest args)
  "Define modeline with NAME and ARGS."
  (declare (indent 1) (doc-string 2))
  (let* ((-if (or (plist-get args :if) t))
         (-format (plist-get args :format))
         (-icon-format (plist-get args :icon-format))
         (-priority (or (plist-get args :priority)
                        (cdr (assq name maple-modeline-priority)) 10))
         (-name (format "%s" name))
         (-func (format "maple-modeline-%s" -name))
         (-show (format "maple-modeline-%s-p" -name))
         (-showf (format "maple-modeline-%s-show-p" -name)))
    `(progn
       (puthash ,-name ,-priority maple-modeline-priority-table)
       (defcustom ,(intern -show) t
         (format "Whether show %s on modeline." ,-name)
         :group 'maple-modeline
         :type 'boolean)
       (defun ,(intern -showf) ()
         (and ,-if ,(intern -show)
              (<= (gethash ,-name maple-modeline-priority-table) 10)))
       (defun ,(intern -func) (&optional face left right)
         (when (,(intern -showf))
           (maple-modeline-raw
            (or (when maple-modeline-icon ,-icon-format) ,-format)
            face left right))))))

(defmacro maple-modeline-flycheck-define (state)
  "Return flycheck information for the given error type STATE."
  `(let* ((counts (flycheck-count-errors flycheck-current-errors))
          (errorp (flycheck-has-current-errors-p ,state))
          (face (intern (format "flycheck-fringe-%s" (symbol-name ,state))))
          (err (or (cdr (assq ,state counts)) "?"))
          (running (eq 'running flycheck-last-status-change)))
     (if (or errorp running)
         (propertize (format "•%s" err) 'face face) "")))

(defun maple-modeline--unicode-number (num)
  "Return a nice unicode representation of a single-digit number NUM."
  (cond
   ((not num) "")
   ((= 1 num) "➊")
   ((= 2 num) "➋")
   ((= 3 num) "➌")
   ((= 4 num) "➍")
   ((= 5 num) "➎")
   ((= 6 num) "➏")
   ((= 7 num) "➐")
   ((= 8 num) "➑")
   ((= 9 num) "➒")
   ((= 10 num) "➓")
   (t (int-to-string num))))

(maple-modeline-define bar
  :if (display-graphic-p)
  :format
  (maple-xpm-draw 'cursor face nil nil nil 'bar))

(maple-modeline-define window-number
  :if (bound-and-true-p window-numbering-mode)
  :format
  (maple-modeline--unicode-number (window-numbering-get-number)))

(maple-modeline-define major-mode
  :format
  (propertize (format-mode-line mode-name)
              'mouse-face 'mode-line-highlight
              'local-map mode-line-major-mode-keymap))

(maple-modeline-define minor-mode
  :format
  (format-mode-line 'minor-mode-alist))

(maple-modeline-define buffer-info
  :format
  (format
   "%s %s %s"
   (propertize
    (if (and maple-modeline-icon buffer-read-only)
        (maple-modeline-icon "lock" face :height 0.9 :v-adjust 0.05) "%*")
    'mouse-face face
    'help-echo "mouse-1: Toggle read only mode."
    'local-map (make-mode-line-mouse-map 'mouse-1 'read-only-mode))
   "%I" (string-trim (format-mode-line mode-line-buffer-identification))))

(maple-modeline-define remote-host
  :if (and default-directory (file-remote-p default-directory 'host))
  :format
  (concat (file-remote-p default-directory 'method) "@" (file-remote-p default-directory 'host)))

(maple-modeline-define buffer-encoding
  :format
  (let* ((buf-coding (format "%s" buffer-file-coding-system))
         (buf-coding (if (string-match "\\(dos\\|unix\\|mac\\)" buf-coding)
                         (match-string 1 buf-coding) buf-coding)))
    (propertize
     (upcase buf-coding)
     'mouse-face 'mode-line-highlight
     'local-map mode-line-coding-system-map)))

(maple-modeline-define buffer-position
  :format
  (propertize
   (format-mode-line
    '((line-number-mode
       (column-number-mode
        (column-number-indicator-zero-based
         "%l:%C" "%l:%c")
        "L%l" "")
       (column-number-mode
        (column-number-indicator-zero-based
         "C%C" "C%c")))))
   'local-map mode-line-column-line-number-mode-map
   'mouse-face 'mode-line-highlight
   'help-echo "Line number and Column number\nmouse-1: Display Line and Column Mode Menu"))

(maple-modeline-define count
  :format
  (maple-modeline-raw '(buffer-encoding :left "" :right (buffer-position :left " | " :right ""))))

(maple-modeline-define position
  :format
  (replace-regexp-in-string "%" "%%" (string-trim-left (format-mode-line "%p "))))

(defsubst maple-modeline-column (pos)
  "Get the column of the position `POS'."
  (save-excursion (goto-char pos) (current-column)))

(maple-modeline-define region
  :if (or (region-active-p) (and (bound-and-true-p evil-local-mode) (eq 'visual evil-state)))
  :format
  (let* ((lines (count-lines (region-beginning) (min (1+ (region-end)) (point-max))))
         (chars (- (1+ (region-end)) (region-beginning)))
         (cols (1+ (abs (- (maple-modeline-column (region-end))
                           (maple-modeline-column (region-beginning))))))
         (evil (and (bound-and-true-p evil-state) (eq 'visual evil-state)))
         (rect (or (bound-and-true-p rectangle-mark-mode)
                   (and (bound-and-true-p evil-visual-selection)
                        (eq 'block evil-visual-selection))))
         (multi-line (or (> lines 1) (and evil (eq 'line evil-visual-selection)))))
    (cond (rect (format "%d×%d block" lines (if evil cols (1- cols))))
          (multi-line (format "%d lines" lines))
          (t (format "%d chars" (if evil chars (1- chars)))))))

(maple-modeline-define misc-info
  :format (string-trim (format-mode-line mode-line-misc-info)))

(maple-modeline-define python
  :if (eq 'python-mode major-mode)
  :format
  (string-trim (cond ((bound-and-true-p pyvenv-virtual-env-name)
                      pyvenv-virtual-env-name)
                     ((bound-and-true-p pyenv-mode-mode-line-format)
                      (format-mode-line pyenv-mode-mode-line-format))
                     (t ""))))

(maple-modeline-define flycheck
  :if (bound-and-true-p flycheck-mode)
  :format
  (concat
   (maple-modeline-flycheck-define 'info)
   (maple-modeline-flycheck-define 'warning)
   (maple-modeline-flycheck-define 'error)))

(maple-modeline-define version-control
  :if (and vc-mode (vc-state (buffer-file-name)))
  :format
  (string-trim (format-mode-line '(vc-mode vc-mode)))
  :icon-format
  (let* ((backend (vc-backend buffer-file-name))
         (state   (vc-state buffer-file-name backend)))
    (format "%s %s"
            (cond ((memq state '(edited added))
                   (maple-modeline-icon "git-compare" face))
                  ((eq state 'needs-merge)
                   (maple-modeline-icon "git-merge" face))
                  ((eq state 'needs-update)
                   (maple-modeline-icon "arrow-down" face))
                  ((memq state '(removed conflict unregistered))
                   (maple-modeline-icon "alert" face))
                  (t
                   (maple-modeline-icon "git-branch" face)))
            (substring vc-mode (+ (if (eq backend 'Hg) 2 3) 2)))))

(maple-modeline-define process
  :format
  (format-mode-line mode-line-process))

(maple-modeline-define narrow
  :if (buffer-narrowed-p)
  :format
  (propertize
   (if maple-modeline-icon (all-the-icons-faicon "filter" :v-adjust 0.1) "Narrow")
   'face 'maple-modeline-active2
   'mouse-face face
   'help-echo "mouse-1: Remove narrowing from the current buffer"
   'local-map (make-mode-line-mouse-map 'mouse-1 'mode-line-widen)))

(maple-modeline-define nyan
  :if (bound-and-true-p nyan-mode)
  :format (nyan-create))

(maple-modeline-define anzu
  :if (bound-and-true-p anzu--state)
  :format (anzu--update-mode-line))

(maple-modeline-define lsp
  :if (bound-and-true-p lsp-mode)
  :format (string-trim (lsp-mode-line)))

(maple-modeline-define iedit
  :if (bound-and-true-p iedit-mode)
  :format
  (format "(%d/%d iedit)" (iedit-update-index) (iedit-counter)))

(maple-modeline-define projectile
  :if (bound-and-true-p projectile-mode)
  :format
  (let ((projectile-mode-line-prefix "")) (funcall projectile-mode-line-function)))

(maple-modeline-define macro
  :if (or defining-kbd-macro executing-kbd-macro)
  :format "•REC")

(maple-modeline-define message
  :format
  (when maple-modeline--message (string-trim maple-modeline--message)))

(maple-modeline-set standard
  :left '((window-number :left (bar :left "")) macro iedit anzu buffer-info major-mode flycheck version-control remote-host region)
  :right '(message narrow python lsp misc-info process count position))

(maple-modeline-set minimal
  :left '((window-number :left (bar :left "")) buffer-info major-mode region)
  :right '(count misc-info position))

(maple-modeline-set sidebar
  :left '((window-number :left (bar :left "")))
  :right '(major-mode))

(defun maple-modeline--init ()
  "Setup the modeline."
  (if maple-modeline-message-p
      (unless maple-modeline--message-active
        (advice-add #'message :around #'maple-modeline-message-around)
        (setq maple-modeline--message-active t))
    (setq maple-modeline--message nil)
    (advice-remove #'message #'maple-modeline-message-around)
    (when (timerp maple-modeline--message-timer)
      (cancel-timer maple-modeline--message-timer)))

  (maple-modeline-reset
   (intern (format "maple-modeline-format-%s"
                   (symbol-name (if (and maple-modeline-side-style (maple-modeline--is-side-window))
                                    maple-modeline-side-style maple-modeline-style))))
   (cond ((eq maple-modeline-width 'auto) nil)
         ((eq maple-modeline-width 'standard) 9999)
         (t maple-modeline-width))))

;;;###autoload
(define-minor-mode maple-modeline-mode
  "Toggle maple-modeline on or off."
  :group 'maple-modeline
  :global t
  (if maple-modeline-mode
      (progn (setq maple-modeline--format mode-line-format
                   mode-line-format '(:eval (maple-modeline--init)))

             (add-hook 'focus-in-hook 'maple-modeline-set-selected-window)
             (add-hook 'focus-out-hook 'maple-modeline-unset-selected-window)
             (add-hook 'window-configuration-change-hook 'maple-modeline-set-selected-window)
             (advice-add 'handle-switch-frame :after 'maple-modeline-set-selected-window)
             (advice-add 'select-frame :after 'maple-modeline-set-selected-window))
    (setq mode-line-format maple-modeline--format
          maple-modeline--format nil)

    (remove-hook 'focus-in-hook 'maple-modeline-set-selected-window)
    (remove-hook 'focus-out-hook 'maple-modeline-unset-selected-window)
    (remove-hook 'window-configuration-change-hook 'maple-modeline-set-selected-window)
    (advice-remove 'handle-switch-frame 'maple-modeline-set-selected-window)
    (advice-remove 'select-frame 'maple-modeline-set-selected-window))
  (setf (default-value 'mode-line-format) mode-line-format))

(provide 'maple-modeline)
;;; maple-modeline.el ends here
