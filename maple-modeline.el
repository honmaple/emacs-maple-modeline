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
  :group 'mode-line)

(defcustom maple-modeline-style 'standard
  "Maple-modeline Style."
  :group 'maple-modeline
  :type '(choice (const standard)
                 (const minimal)))

(defcustom maple-modeline-width 'reset
  "Maple-modeline witdh."
  :group 'maple-modeline
  :type '(choice (const standard)
                 (const reset)))

(defcustom maple-modeline-background (if (display-graphic-p) "#35331D" "#333333")
  "Maple-modeline background color."
  :group 'maple-modeline
  :type 'string)

(defcustom maple-modeline-direction (if (display-graphic-p) '(auto . auto) '(right . left))
  "Maple-modeline show direction."
  :group 'maple-modeline
  :type 'cons)

(defface maple-modeline-active0 '((t (:inherit mode-line)))
  "Maple-modeline active face 0."
  :group 'maple-modeline)

(defface maple-modeline-active1 `((t (:background ,maple-modeline-background :foreground "white" :inherit mode-line)))
  "Maple-modeline active face 1."
  :group 'maple-modeline)

(defface maple-modeline-active2
  '((t (:foreground "plum1" :distant-foreground "DarkMagenta" :weight bold)))
  "Face for highlighting the python venv."
  :group 'maple-modeline)

(defface maple-modeline-inactive0 '((t (:inherit mode-line-inactive)))
  "Maple-modeline inactive face 0."
  :group 'maple-modeline)

(defface maple-modeline-inactive1
  `((t (:background ,maple-modeline-background :inherit mode-line-inactive)))
  "Maple-modeline inactive face 1."
  :group 'maple-modeline)

(defun maple-modeline-render(s face0 face1 &optional sep direction start-direction prepend append1)
  "S FACE0 FACE1 &OPTIONAL SEP DIRECTION START-DIRECTION PREPEND APPEND1."
  (let* ((reverse (not (or (eq direction 'right)
                           (and (eq direction 'auto)
                                (eq start-direction 'right)))))
         (r (cl-loop for z in s append
                     (let* ((str (if (symbolp z)
                                     (funcall (intern (format "maple-modeline--%s" z)) face1)
                                   (maple-modeline-raw z face1)))
                            (typ (if (symbolp z)
                                     (or (not str) (string= (string-trim str) ""))
                                   (string= str "")))
                            (s (or sep (maple-xpm-draw face0 face1 reverse))))
                       (when (not typ)
                         (when (eq direction 'auto)
                           (setq reverse (not reverse)))
                         (setq face0 (prog1 face1 (setq face1 face0)))
                         (list s str))))))
    (append (if prepend r (cdr r))
            (when append1
              (list (or sep (maple-xpm-draw face0 face1 reverse)))))))

(defun maple-modeline-display(l r &optional sep)
  "L R &OPTIONAL SEP PREPEND APPEND."
  (let* ((active (maple-modeline--active))
         (face0  (if active 'maple-modeline-active0 'maple-modeline-inactive0))
         (face1  (if active 'maple-modeline-active1 'maple-modeline-inactive1))
         (ld (car maple-modeline-direction))
         (rd (cdr maple-modeline-direction))
         (lv (maple-modeline-render l face0 face1 sep ld nil nil t))
         (rv (maple-modeline-render r face0 face1 sep rd nil t nil))
         (cv (maple-modeline-fill (+ 2 (string-width (format-mode-line (string-join rv ""))))))
         (rv (if (cl-evenp (/ (length lv) 2))
                 (maple-modeline-render (append (list cv) r) face0 face1 sep rd)
               (maple-modeline-render (append (list cv) r) face1 face0 sep rd 'right))))
    (string-join (append lv rv) "")))

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
  (propertize " "
              'display `((space :align-to (- (+ right right-fringe right-margin) ,reserve)))))

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
         (-face (plist-get args :face))
         (-name (format "%s" name))
         (-priority (or (plist-get args :priority) 100)))
    `(progn
       (puthash ,-name ,-priority maple-modeline-priority-table)
       (defvar ,(intern (format "maple-modeline-%s-p" -name)) t)
       (defun ,(intern (format "maple-modeline--%s" -name)) (&optional face)
         (when  (and ,-if
                     ,(intern (format "maple-modeline-%s-p" name))
                     (<= (gethash ,-name maple-modeline-priority-table) 100))
           (if ,-face (maple-modeline-raw ,-format (list ,-face face))
             (maple-modeline-raw ,-format face)))))))

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
  (let ((color (face-attribute 'cursor :background)))
    (maple-modeline--add-text-property
     (maple-modeline--unicode-number
      (int-to-string (window-numbering-get-number)))
     'face `(:foreground ,color :distant-foreground "white"))))

(maple-modeline-define major-mode
  :priority 74
  :format
  (propertize (format-mode-line mode-name)
              'mouse-face 'mode-line-highlight
              'local-map mode-line-major-mode-keymap))

(maple-modeline-define buffer-info
  :priority 78
  :format
  (format "%s %s %s" "%*" "%I"
          (string-trim (format-mode-line mode-line-buffer-identification))))

(maple-modeline-define count
  :priority 75
  :format
  (format "%s | %s:%s"
          (let ((buf-coding (format "%s" buffer-file-coding-system)))
            (upcase (if (string-match "\\(dos\\|unix\\|mac\\)" buf-coding)
                        (match-string 1 buf-coding) buf-coding))) "%l" "%c"))

(maple-modeline-define selection-info
  :if (region-active-p)
  :priority 74
  :face 'maple-modeline-active2
  :format
  (let* ((lines (count-lines (region-beginning) (min (1+ (region-end)) (point-max))))
         (chars (- (1+ (region-end)) (region-beginning)))
         (multi-line (> lines 1)))
    (cond (multi-line (format "%d lines" lines))
          (t (format "%d chars" chars)))))

(maple-modeline-define misc-info
  :priority 78
  :format (string-trim (format-mode-line mode-line-misc-info)))

(maple-modeline-define screen
  :priority 80
  :format "%p")

(maple-modeline-define python-pyvenv
  :if (and (eq 'python-mode major-mode)
           (bound-and-true-p pyvenv-virtual-env-name))
  :face 'maple-modeline-active2
  :format pyvenv-virtual-env-name)

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
  :face 'mode-line-buffer-id
  :format (format "(%d/%d iedit)" (iedit-update-index) (iedit-counter)))

(maple-modeline-define macro
  :if (or defining-kbd-macro executing-kbd-macro)
  :priority 78
  :face 'mode-line-buffer-id
  :format "•REC")

(maple-modeline-set standard
  :left '(window-number macro iedit anzu buffer-info major-mode flycheck version-control selection-info)
  :right '(process python-pyvenv count misc-info screen))

(maple-modeline-set minimal
  :left '(window-number major-mode buffer-info selection-info)
  :right '(count misc-info screen))

;;;###autoload
(defun maple-modeline-init ()
  "Setup the default modeline."
  (interactive)
  (let* ((f (intern (format "maple-modeline-format-%s"
                            (symbol-name maple-modeline-style))))
         (width (cond ((eq maple-modeline-width 'reset) nil)
                      ((eq maple-modeline-width 'standard) 9999)
                      (t maple-modeline-width))))
    (setq-default mode-line-format `("%e" (:eval (maple-modeline-reset ',f ,width))))))

(provide 'maple-modeline)
;;; maple-modeline.el ends here
