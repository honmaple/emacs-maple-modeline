;;; maple-modeline-segments.el --- Modeline segment configurations	-*- lexical-binding: t -*-

;; Copyright (C) 2015-2026 lin.jiang

;; Author: lin.jiang <mail@honmaple.com>
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
;; modeline segments.
;;

;;; Code:
(require 'maple-modeline-core)
(require 'maple-modeline-separators)

(defvar evil-state)
(defvar evil-visual-selection)
(defvar flycheck-current-errors)
(defvar flycheck-last-status-change)

(declare-function flycheck-count-errors 'flycheck)
(declare-function flycheck-has-current-errors-p 'flycheck)
(declare-function flymake--mode-line-counter 'flymake)
(declare-function nerd-icons-icon-for-file 'nerd-icons)
(declare-function nerd-icons--function-name 'nerd-icons)

(defcustom maple-modeline-priority-alist
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
    (window-number . 10)
    (bar . 10))
  "Maple-modeline priority define."
  :type '(alist :key-type symbol :value-type integer)
  :group 'maple-modeline)

(defcustom maple-modeline-segment-alist nil
  "Maple-modeline segment list."
  :type '(alist :key-type symbol :value-type function)
  :group 'maple-modeline)

(defmacro maple-modeline-define-segment (name &rest args)
  "Define modeline with NAME and ARGS."
  (declare (indent 1) (doc-string 2))
  (let* ((-if (or (plist-get args :if) t))
         (-format (plist-get args :format))
         (-priority (plist-get args :priority))
         (-name (format "%s" name))
         (-func (intern (format "maple-modeline-segment--%s" -name)))
         forms)
    (dolist (x (plist-get args :defines))
      (push `(defvar ,x) forms))
    (dolist (x (plist-get args :functions))
      (push `(declare-function ,x ',name) forms))
    `(progn
       ,@forms
       (add-to-list 'maple-modeline-segment-alist (cons ',name ',-func))
       (when ,-priority
         (add-to-list 'maple-modeline-priority-alist (cons ',name ,-priority)))
       (defun ,-func (&optional face left right)
         (when (and ,-if (> (or (cdr (assq ',name maple-modeline-priority-alist)) 10) 0))
           (maple-modeline--format-segment ,-format face left right))))))

(defun maple-modeline--concat-or(&rest args)
  "Return first arg of ARGS that not is not empty."
  (cl-loop for arg in args
           when (and arg (not (string= arg "")))
           return arg))

(defun maple-modeline--concat-and(&rest args)
  "Concat ARGS when prev arg is not empty."
  (cl-loop for arg in args
           until (or (not arg) (string= arg ""))
           concat arg into result
           finally return result))

(defun maple-modeline--icon(icon-set icon-name face &rest args)
  "Render icon with ICON-NAME of ICON-SET and FACE ARGS."
  (when (and maple-modeline-icon (featurep 'nerd-icons))
    (let ((func (nerd-icons--function-name icon-set)))
      (and (fboundp func) (apply func (append (list icon-name) args (list :v-adjust -0.05 :face face)))))))

(defun maple-modeline--icon-propertize (icon &optional face)
  "Propertize the ICON with the specified FACE."
  (when icon
    (propertize
     icon
     'face `(:inherit ,face ,@(get-text-property 0 'face icon))
     'display `(raise -0.05))))

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
       (propertize mm 'face (append (if (listp cur) cur (list cur)) (if (listp val) val (list val))))))
   (maple-modeline--property-substrings str prop) ""))

(defun maple-modeline--format-segment(segment &optional face left-segment right-segment)
  "Format SEGMENT as mode-line data with FACE LEFT-SEGMENT RIGHT-SEGMENT."
  (let ((left (cond ((not left-segment) " ")
                    ((stringp left-segment) left-segment)
                    (t (maple-modeline--format-segment left-segment face))))
        (right (cond ((not right-segment) " ")
                     ((stringp right-segment) right-segment)
                     (t (maple-modeline--format-segment right-segment face)))))
    (cond ((maple-modeline--is-empty segment) "")
          ((symbolp segment)
           (let ((func (cdr (assq segment maple-modeline-segment-alist))))
             (unless func
               (error "Miss segment define: %s" segment))
             (funcall func face left-segment right-segment)))
          ((listp segment)
           (let ((args (cdr segment)))
             (maple-modeline--format-segment (car segment) face (plist-get args :left) (plist-get args :right))))
          (t (let ((padded-str (concat left segment right)))
               (if face (maple-modeline--add-text-property padded-str 'face face) padded-str))))))

(defun maple-modeline--format-flycheck (state)
  "Return flycheck information for the given error type STATE."
  (let* ((counts (flycheck-count-errors flycheck-current-errors))
         (errorp (flycheck-has-current-errors-p state))
         (face (intern (format "flycheck-fringe-%s" (symbol-name state))))
         (err (or (cdr (assq state counts)) "?"))
         (running (eq 'running flycheck-last-status-change)))
    (if (or errorp running)
        (propertize (format "•%s" err) 'face face) "")))

(defun maple-modeline--format-flymake (type)
  "Return flymake information for the given error TYPE."
  (let ((count 0))
    (dolist (d (flymake-diagnostics))
      (when (= (flymake--severity type) (flymake--severity (flymake-diagnostic-type d)))
        (cl-incf count)))
    (if (> count 0)
        (propertize (format "•%s" count) 'face (flymake--lookup-type-property type 'mode-line-face 'compilation-error)) "")))

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

(maple-modeline-define-segment bar
  :if (display-graphic-p)
  :format
  (let ((color0 (maple-modeline--background 'cursor))
        (color1 (maple-modeline--background face)))
    (maple-modeline-separator-draw 'bar color0 color1)))

(maple-modeline-define-segment evil
  :if (bound-and-true-p evil-local-mode)
  :format
  (pcase evil-state
    ('normal
     (maple-modeline--concat-or (maple-modeline--icon 'mdicon "nf-md-alpha_n_circle" face) "🅝"))
    ('insert
     (maple-modeline--concat-or (maple-modeline--icon 'mdicon "nf-md-alpha_i_circle" face) "🅘"))
    ('visual
     (maple-modeline--concat-or (maple-modeline--icon 'mdicon "nf-md-alpha_v_circle" face) "🅥"))
    ('emacs
     (maple-modeline--concat-or (maple-modeline--icon 'mdicon "nf-md-alpha_e_circle" face) "🅔"))
    ('motion
     (maple-modeline--concat-or (maple-modeline--icon 'mdicon "nf-md-alpha_m_circle" face) "🅜"))
    ('replace
     (maple-modeline--concat-or (maple-modeline--icon 'mdicon "nf-md-alpha_r_circle" face) "🅡"))
    ('operator
     (maple-modeline--concat-or (maple-modeline--icon 'mdicon "nf-md-alpha_o_circle" face) "🅞"))
    (_ (upcase (substring (symbol-name evil-state) 0 1)))))

(maple-modeline-define-segment window-number
  :functions (window-numbering-get-number)
  :if (bound-and-true-p window-numbering-mode)
  :format
  (maple-modeline--unicode-number (window-numbering-get-number)))

(maple-modeline-define-segment major-mode
  :format
  (propertize (format-mode-line mode-name)
              'mouse-face 'mode-line-highlight
              'local-map mode-line-major-mode-keymap))

(maple-modeline-define-segment minor-mode
  :format
  (format-mode-line 'minor-mode-alist))

(maple-modeline-define-segment buffer-info
  :format
  (format
   "%s %s %s"
   (propertize
    (maple-modeline--concat-or
     (when buffer-read-only
       (maple-modeline--icon 'mdicon "nf-md-lock" face :height 0.9 :v-adjust 0.05))
     "%*")
    'mouse-face face
    'help-echo "mouse-1: Toggle read only mode."
    'local-map (make-mode-line-mouse-map 'mouse-1 'read-only-mode))
   "%I"
   (concat
    (when (and maple-modeline-icon (buffer-file-name))
      (maple-modeline--concat-and
       (maple-modeline--icon-propertize (nerd-icons-icon-for-file (file-name-nondirectory (buffer-file-name))) face)
       " "))
    (string-trim (format-mode-line mode-line-buffer-identification)))))

(maple-modeline-define-segment remote-host
  :if (and default-directory (file-remote-p default-directory 'host))
  :format
  (concat (file-remote-p default-directory 'method) "@" (file-remote-p default-directory 'host)))

(maple-modeline-define-segment buffer-encoding
  :format
  (let* ((buf-coding (format "%s" buffer-file-coding-system))
         (buf-coding (if (string-match "\\(dos\\|unix\\|mac\\)" buf-coding)
                         (match-string 1 buf-coding) buf-coding)))
    (propertize
     (upcase buf-coding)
     'mouse-face 'mode-line-highlight
     'local-map mode-line-coding-system-map)))

(maple-modeline-define-segment buffer-position
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

(maple-modeline-define-segment count
  :format
  (maple-modeline--format-segment '(buffer-encoding :left "" :right (buffer-position :left " | " :right ""))))

(maple-modeline-define-segment position
  :format
  (replace-regexp-in-string "%" "%%" (string-trim-left (format-mode-line "%p "))))

(defsubst maple-modeline-column (pos)
  "Get the column of the position `POS'."
  (save-excursion (goto-char pos) (current-column)))

(maple-modeline-define-segment region
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

(maple-modeline-define-segment misc-info
  :format (string-trim (format-mode-line mode-line-misc-info)))

(maple-modeline-define-segment python
  :defines (pyvenv-virtual-env-name)
  :if (eq 'python-mode major-mode)
  :format
  (string-trim (cond ((bound-and-true-p pyvenv-virtual-env-name)
                      pyvenv-virtual-env-name)
                     ((bound-and-true-p pyenv-mode-mode-line-format)
                      (format-mode-line pyenv-mode-mode-line-format))
                     (t ""))))

(maple-modeline-define-segment flymake
  :if (bound-and-true-p flymake-mode)
  :format
  (concat
   (maple-modeline--format-flymake :error)
   (maple-modeline--format-flymake :warning)
   (maple-modeline--format-flymake :note)))

(maple-modeline-define-segment flycheck
  :if (bound-and-true-p flycheck-mode)
  :format
  (concat
   (maple-modeline--format-flycheck 'info)
   (maple-modeline--format-flycheck 'warning)
   (maple-modeline--format-flycheck 'error)))

(maple-modeline-define-segment version-control
  :if (and vc-mode (vc-state (buffer-file-name)))
  :format
  (maple-modeline--concat-or
   (when maple-modeline-icon
     (let* ((backend (vc-backend buffer-file-name))
            (state   (vc-state buffer-file-name backend)))
       (concat
        (maple-modeline--concat-and
         (cond ((memq state '(edited added))
                (maple-modeline--icon 'devicon "nf-dev-git_compare" face))
               ((eq state 'needs-merge)
                (maple-modeline--icon 'devicon "nf-dev-git_merge" face))
               ((eq state 'needs-update)
                (maple-modeline--icon 'devicon "nf-dev-git_pull_request" face))
               ((memq state '(removed conflict unregistered))
                (maple-modeline--icon 'mdicon "nf-md-alert" face))
               (t
                (maple-modeline--icon 'devicon "nf-dev-git_branch" face)))
         " ")
        (substring vc-mode (+ (if (eq backend 'Hg) 2 3) 2)))))
   (string-trim (format-mode-line '(vc-mode vc-mode)))))

(maple-modeline-define-segment process
  :format
  (format-mode-line mode-line-process))

(maple-modeline-define-segment narrow
  :if (buffer-narrowed-p)
  :format
  (propertize
   (maple-modeline--concat-or
    (maple-modeline--icon 'octicon "nf-oct-fold" face :v-adjust 0.1)
    "Narrow")
   'face 'maple-modeline-active2
   'mouse-face face
   'help-echo "mouse-1: Remove narrowing from the current buffer"
   'local-map (make-mode-line-mouse-map 'mouse-1 'mode-line-widen)))

(maple-modeline-define-segment nyan
  :functions (nyan-create)
  :if (bound-and-true-p nyan-mode)
  :format (nyan-create))

(maple-modeline-define-segment anzu
  :functions (anzu--update-mode-line)
  :if (bound-and-true-p anzu--state)
  :format (anzu--update-mode-line))

(maple-modeline-define-segment lsp
  :functions (lsp-workspaces lsp--workspace-print)
  :if (bound-and-true-p lsp-mode)
  :format
  (let ((workspaces (lsp-workspaces)))
    (concat "LSP" (if workspaces (string-join (mapcar (lambda (w) (format "[%s]" (lsp--workspace-print w))) workspaces))
                    (propertize "[Disconnected]" 'face 'warning)))))

(maple-modeline-define-segment iedit
  :functions (iedit-update-index iedit-counter)
  :if (bound-and-true-p iedit-mode)
  :format
  (format "(%d/%d iedit)" (iedit-update-index) (iedit-counter)))

(maple-modeline-define-segment projectile
  :defines (projectile-mode-line-prefix projectile-mode-line-function)
  :if (bound-and-true-p projectile-mode)
  :format
  (let ((projectile-mode-line-prefix "")) (funcall projectile-mode-line-function)))

(maple-modeline-define-segment macro
  :if (or defining-kbd-macro executing-kbd-macro)
  :format "• REC")

(provide 'maple-modeline-segments)
;;; maple-modeline-segments.el ends here