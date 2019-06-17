;;; maple-modeline.el --- modeline configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2015-2019 lin.jiang

;; Author: lin.jiang <mail@honmaple.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "25.1"))
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
(require 'subr-x)
(require 'maple-modeline-window)
(require 'maple-xpm)

(defvar pyvenv-virtual-env-name)

(defgroup maple-modeline nil
  "Maple-modeline, a prettier mode line."
  :group 'maple)

(defcustom maple-modeline-style 'standard
  "Maple-modeline Style."
  :group 'maple-modeline
  :type '(choice (const standard)
                 (const minimal)
                 (const sidebar)))

(defcustom maple-modeline-width 'reset
  "Maple-modeline witdh."
  :group 'maple-modeline
  :type '(choice (const standard)
                 (const reset)))

(defcustom maple-modeline-sep 'maple-xpm-draw
  "Maple-modeline draw separator func."
  :group 'maple-modeline
  :type 'func)

(defcustom maple-modeline-background (if (display-graphic-p) "#35331D" "#333333")
  "Maple-modeline background color."
  :group 'maple-modeline
  :type 'string)

(defcustom maple-modeline-direction (if (display-graphic-p) '(auto . auto) '(right . left))
  "Maple-modeline show direction."
  :group 'maple-modeline
  :type 'cons)

(defcustom maple-modeline-face '((window-number . maple-modeline-evil-face))
  "Maple-modeline face define."
  :group 'maple-modeline
  :type '(cons))

(defface maple-modeline-active0
  '((t (:inherit mode-line)))
  "Maple-modeline active face 0."
  :group 'maple-modeline)

(defface maple-modeline-active1
  `((t (:inherit mode-line :background ,maple-modeline-background :foreground "white")))
  "Maple-modeline active face 1."
  :group 'maple-modeline)

(defface maple-modeline-active2
  '((t (:foreground "plum1" :distant-foreground "DarkMagenta" :weight bold)))
  "Face for highlighting the python venv."
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
  (let ((background (face-attribute 'cursor :background)))
    `(:background ,background :foreground "#3E3D31")))

(defun maple-modeline-evil-face(face)
  "Get evil face from cursor with default FACE."
  (let ((foreground (face-attribute 'cursor :background))
        (background (face-attribute face :background)))
    `(:foreground ,foreground :background ,background)))

(defun maple-modeline-render(s face0 face1 &optional sep direction start-direction prepend append1)
  "S FACE0 FACE1 &OPTIONAL SEP DIRECTION START-DIRECTION PREPEND APPEND1."
  (let* ((reverse (not (or (eq direction 'right)
                           (and (eq direction 'auto)
                                (eq start-direction 'right)))))
         (face nil)
         (r (cl-loop for z in s append
                     (let* ((f (cdr (assq z maple-modeline-face)))
                            (face2 (if f (if (facep f) f (funcall f face1)) face1))
                            (str (if (symbolp z)
                                     (funcall (intern (format "maple-modeline--%s" z)) face2)
                                   (maple-modeline-raw z face2)))
                            (typ (if (symbolp z)
                                     (or (not str) (string= (string-trim str) ""))
                                   (string= str "")))
                            (sp (or sep (funcall maple-modeline-sep (or face face0) face2 reverse))))
                       (unless typ
                         (when (eq direction 'auto)
                           (setq reverse (not reverse)))
                         (setq face (when f face2))
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
         (cv (maple-modeline-fill (string-width (format-mode-line (string-join rv "")))))
         (rrv (if (cl-evenp (/ (length lv) 2))
                  (maple-modeline-render (append (list cv) r) face0 face1 sep rd)
                (maple-modeline-render (append (list cv) r) face1 face0 sep rd 'right))))
    (cond ((not (car r)) (string-join lv ""))
          ((not (car l)) (string-join (append (list cv) rv) ""))
          (t (string-join (append lv rrv) "")))))

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

(defun maple-modeline-raw (str &rest face)
  "Render STR as mode-line data with FACE."
  (let* ((rendered-str (format-mode-line str))
         (padded-str (concat " " (if (listp str) rendered-str str) " ")))
    (if face (maple-modeline--add-text-property padded-str 'face face) padded-str)))

(defun maple-modeline-fill (reserve)
  "Return empty space leaving RESERVE space on the right."
  (unless reserve
    (setq reserve 20))
  (when (and window-system (eq 'right (get-scroll-bar-mode)))
    (setq reserve (- reserve 3)))
  (propertize " " 'display `((space :align-to (- right ,reserve)))))

(defmacro maple-modeline-set (name &rest args)
  "Set modeline with NAME and ARGS."
  (declare (indent 1)
           (doc-string 2))
  (let* ((-name (format "%s" name))
         (-left (or (plist-get args :left) '()))
         (-right (or (plist-get args :right) '()))
         (-sep (plist-get args :sep)))
    `(defun ,(intern (format "maple-modeline-format-%s" -name)) ()
       (maple-modeline-display ,-left ,-right ,-sep))))

(defmacro maple-modeline-define (name &rest args)
  "Define modeline with NAME and ARGS."
  (declare (indent 1)
           (doc-string 2))
  (let* ((-if (or (plist-get args :if) t))
         (-format (plist-get args :format))
         (-name (format "%s" name))
         (-func (format "maple-modeline--%s" -name))
         (-show (format "maple-modeline-%s-p" -name))
         (-showf (format "maple-modeline-%s-show-p" -name))
         (-priority (or (plist-get args :priority)
                        (gethash -name maple-modeline-priority-table) 100)))
    `(progn
       (puthash ,-name ,-priority maple-modeline-priority-table)
       (defvar ,(intern -show) t)
       (defun ,(intern -showf) ()
         (and ,-if ,(intern -show)
              (<= (gethash ,-name maple-modeline-priority-table) 100)))
       (defun ,(intern -func) (&optional face)
         (when (,(intern -showf))
           (maple-modeline-raw ,-format face))))))

(defmacro maple-modeline-flycheck-define (state)
  "Return flycheck information for the given error type STATE."
  `(let* ((counts (flycheck-count-errors flycheck-current-errors))
          (errorp (flycheck-has-current-errors-p ,state))
          (face (intern (format "flycheck-fringe-%s" (symbol-name ,state))))
          (err (or (cdr (assq ,state counts)) "?"))
          (running (eq 'running flycheck-last-status-change)))
     (if (or errorp running)
         (propertize (format "•%s" err) 'face face) "")))

(defun maple-modeline--unicode-number (str)
  "Return a nice unicode representation of a single-digit number STR."
  (cond
   ((string= "1" str) "➊")
   ((string= "2" str) "➋")
   ((string= "3" str) "➌")
   ((string= "4" str) "➍")
   ((string= "5" str) "➎")
   ((string= "6" str) "➏")
   ((string= "7" str) "➐")
   ((string= "8" str) "➑")
   ((string= "9" str) "➒")
   ((string= "10" str) "➓")
   (t str)))

(maple-modeline-define window-number
  :if (bound-and-true-p window-numbering-mode)
  :format
  (maple-modeline--unicode-number
   (int-to-string (window-numbering-get-number))))

(maple-modeline-define major-mode
  :priority 74
  :format
  (propertize (format-mode-line mode-name)
              'mouse-face 'mode-line-highlight
              'local-map mode-line-major-mode-keymap))

(maple-modeline-define minor-mode
  :priority 74
  :format
  (format-mode-line 'minor-mode-alist))

(maple-modeline-define buffer-info
  :priority 78
  :format
  (format "%s %s %s" "%*" "%I"
          (string-trim (format-mode-line mode-line-buffer-identification))))

(maple-modeline-define remote-host
  :if (and default-directory (file-remote-p default-directory 'host))
  :format
  (propertize (concat tramp-current-method "@" (file-remote-p default-directory 'host))
              'face 'mode-line-buffer-id))

(maple-modeline-define count
  :priority 75
  :format
  (let ((buf-coding (format "%s" buffer-file-coding-system)))
    (format "%s | %s:%s"
            (upcase (if (string-match "\\(dos\\|unix\\|mac\\)" buf-coding)
                        (match-string 1 buf-coding) buf-coding))
            "%l" "%c")))

(defsubst maple-modeline-column (pos)
  "Get the column of the position `POS'."
  (save-excursion (goto-char pos) (current-column)))

(maple-modeline-define selection-info
  :if (or (region-active-p) (and (bound-and-true-p evil-local-mode) (eq 'visual evil-state)))
  :priority 74
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
    (propertize
     (cond (rect (format "%d×%d block" lines (if evil cols (1- cols))))
           (multi-line (format "%d lines" lines))
           (t (format "%d chars" (if evil chars (1- chars)))))
     'face 'maple-modeline-active2)))

(maple-modeline-define misc-info
  :priority 78
  :format (string-trim (format-mode-line mode-line-misc-info)))

(maple-modeline-define screen
  :priority 80
  :format "%p ")

(maple-modeline-define python-pyvenv
  :if (and (eq 'python-mode major-mode)
           (bound-and-true-p pyvenv-virtual-env-name))
  :priority 70
  :format
  (propertize pyvenv-virtual-env-name 'face 'maple-modeline-active2))

(maple-modeline-define flycheck
  :if (bound-and-true-p flycheck-mode)
  :priority 72
  :format
  (concat
   (maple-modeline-flycheck-define 'info)
   (maple-modeline-flycheck-define 'warning)
   (maple-modeline-flycheck-define 'error)))

(maple-modeline-define version-control
  :if (and vc-mode (vc-state (buffer-file-name)))
  :priority 78
  :format
  (format "%s" (string-trim (format-mode-line '(vc-mode vc-mode)))))

(maple-modeline-define process
  :format
  (propertize (format-mode-line mode-line-process) 'face 'maple-modeline-active2))

(maple-modeline-define anzu
  :if (bound-and-true-p anzu--state)
  :priority 79
  :format (anzu--update-mode-line))

(maple-modeline-define iedit
  :if (bound-and-true-p iedit-mode)
  :priority 79
  :format
  (propertize (format "(%d/%d iedit)" (iedit-update-index) (iedit-counter))
              'face 'mode-line-buffer-id))

(maple-modeline-define macro
  :if (or defining-kbd-macro executing-kbd-macro)
  :priority 78
  :format
  (propertize "•REC" 'face 'mode-line-buffer-id))

(maple-modeline-set standard
  :left '(window-number macro iedit anzu buffer-info major-mode flycheck version-control remote-host selection-info)
  :right '(process python-pyvenv count misc-info screen))

(maple-modeline-set minimal
  :left '(window-number buffer-info major-mode selection-info)
  :right '(count misc-info screen))

(maple-modeline-set sidebar
  :left '(window-number)
  :right '(major-mode))

(defun maple-modeline--init ()
  "Setup the modeline."
  (maple-modeline-reset
   (intern (format "maple-modeline-format-%s" (symbol-name maple-modeline-style)))
   (cond ((eq maple-modeline-width 'reset) nil)
         ((eq maple-modeline-width 'standard) 9999)
         (t maple-modeline-width))))

;;;###autoload
(defun maple-modeline-init ()
  "Setup the default modeline."
  (interactive)
  (setq-default mode-line-format `("%e" (:eval (maple-modeline--init)))))

(provide 'maple-modeline)
;;; maple-modeline.el ends here
