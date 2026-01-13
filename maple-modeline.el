;;; maple-modeline.el --- modeline configurations.	-*- lexical-binding: t -*-

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
;; modeline configurations.
;;

;;; Code:
(require 'maple-modeline-core)
(require 'maple-modeline-segments)
(require 'maple-modeline-separators)

(defvar maple-modeline--selected-window (frame-selected-window))

(defun maple-modeline--is-side-window (&optional window)
  "Check selected window is side WINDOW."
  (memq (window-parameter window 'window-side) '(left right)))

(defun maple-modeline-set-selected-window (&rest _)
  "Set the variable `maple-modeline--selected-window` appropriately."
  (unless (minibuffer-window-active-p (frame-selected-window))
    (setq maple-modeline--selected-window (frame-selected-window))))

(defun maple-modeline-unset-selected-window ()
  "Unsets the variable `maple-modeline--selected-window` and update the modeline."
  (setq maple-modeline--selected-window nil))

(defun maple-modeline--format-separator(separator face0 face1 &optional reverse)
  "SEPARATOR FACE0 FACE1 &OPTIONAL REVERSE."
  (let ((color0 (maple-modeline--separator-color face0))
        (color1 (maple-modeline--separator-color face1))
        (separator (or separator maple-modeline-separator)))
    (cond ((not separator) "")
          ((stringp separator) separator)
          ((functionp separator) (funcall separator color0 color1 reverse))
          (t
           (maple-modeline-separator-draw separator color0 color1 reverse)))))

(defun maple-modeline--format(left-segments right-segments &optional separator)
  "LEFT-SEGMENTS RIGHT-SEGMENTS &OPTIONAL SEPARATOR."
  (let* ((active (eq (selected-window) maple-modeline--selected-window))
         (face0  (if active 'maple-modeline-active0 'maple-modeline-inactive0))
         (face1  (if active 'maple-modeline-active1 'maple-modeline-inactive1))
         left-results right-results)

    (let ((index 0) results)
      (dolist (segment left-segments)
        (let* ((face (maple-modeline--face segment (if (cl-evenp index) face0 face1)))
               (result (maple-modeline--format-segment segment face))
               (reverse (pcase (car maple-modeline-direction)
                          ('left t)
                          ('right nil)
                          ('auto (cl-evenp index)))))
          (unless (maple-modeline--is-empty result)
            (let ((sep (maple-modeline--format-separator
                        separator
                        (or face (if (cl-evenp index) face0 face1))
                        (if (cl-evenp index) face1 face0)
                        reverse)))
              (push (if sep (concat result sep) result) results))
            (setq index (+ index 1)))))
      (setq left-results (reverse results)))

    (let ((index (if (cl-evenp (length left-results)) 0 1)) results)
      (dolist (segment right-segments)
        (let* ((face (maple-modeline--face segment (if (cl-evenp index) face1 face0)))
               (result (maple-modeline--format-segment segment face))
               (reverse (pcase (cdr maple-modeline-direction)
                          ('left t)
                          ('right nil)
                          ('auto (cl-evenp index)))))
          (unless (maple-modeline--is-empty result)
            (let ((sep (maple-modeline--format-separator
                        separator
                        (if (cl-evenp index) face0 face1)
                        (or face (if (cl-evenp index) face1 face0))
                        reverse)))
              (push (if sep (concat sep result) result) results))
            (setq index (+ index 1)))))
      (setq right-results (reverse results)))

    (let* ((left-segment (string-join left-results))
           (right-segment (string-join right-results))
           (right-align-width
            (- (maple-modeline--string-pixel-width right-segment)
               (* (window-font-width nil 'mode-line)
                  (if (and (display-graphic-p) (eq 'right (get-scroll-bar-mode))) 3 1)))))
      (concat
       left-segment
       (maple-modeline--format-segment
        (propertize " " 'display `((space :align-to (- right (,right-align-width)))))
        (if (cl-evenp (length left-results)) face0 face1))
       right-segment))))

(defun maple-modeline--min-priority(segments)
  "Found min priority segment of SEGMENTS."
  (let* ((first (car segments))
         (result (cons first (or (cdr (assq first maple-modeline-priority-alist)) 10))))
    (dolist (segment segments)
      (let ((priority (or (cdr (assq segment maple-modeline-priority-alist)) 10)))
        (when (< priority (cdr result))
          (setq result (cons segment priority)))))
    (car result)))

(defun maple-modeline--adjust(left-segments right-segments &optional separator width)
  "Adjust modeline WIDTH with LEFT-SEGMENTS RIGHT-SEGMENTS SEPARATOR."
  (let* ((max-width (or width (with-current-buffer (current-buffer)
                                (+ (window-width)
                                   (or (cdr (window-margins)) 0)
                                   (or (car (window-margins)) 0)))))
         (result (maple-modeline--format left-segments right-segments separator)))
    (while (> (string-width result) max-width)
      (let ((min-priority-segment (maple-modeline--min-priority (append left-segments right-segments))))
        (setq left-segments (cl-remove-if (lambda(x) (eq x min-priority-segment)) left-segments))
        (setq right-segments (cl-remove-if (lambda(x) (eq x min-priority-segment)) right-segments))
        (setq result (maple-modeline--format left-segments right-segments separator))))
    result))

(defun maple-modeline--init ()
  "The format of modeline."
  (let* ((style (if (and maple-modeline-side-style (maple-modeline--is-side-window))
                    maple-modeline-side-style maple-modeline-style))
         (plist (cdr (assq style maple-modeline-alist))))
    (maple-modeline--adjust
     (maple-modeline--plist plist :left)
     (maple-modeline--plist plist :right)
     (maple-modeline--plist plist :separator)
     (cond ((eq maple-modeline-width 'auto) nil)
           ((eq maple-modeline-width 'standard) 9999)
           ((numberp maple-modeline-width) maple-modeline-width)))))

(defvar maple-modeline--format nil)

(maple-modeline-define standard
  :left ((window-number :left (bar :left "")) macro iedit anzu buffer-info major-mode flycheck flymake version-control remote-host region)
  :right (narrow python lsp misc-info process count position))

(maple-modeline-define minimal
  :left ((window-number :left (bar :left "")) buffer-info major-mode region)
  :right (count misc-info position))

(maple-modeline-define sidebar
  :left ((window-number :left (bar :left "")))
  :right (major-mode))

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
             (advice-add 'select-frame :after 'maple-modeline-set-selected-window)
             (advice-add 'load-theme :after 'maple-modeline--separator-reset))
    (setq mode-line-format maple-modeline--format
          maple-modeline--format nil)

    (remove-hook 'focus-in-hook 'maple-modeline-set-selected-window)
    (remove-hook 'focus-out-hook 'maple-modeline-unset-selected-window)
    (remove-hook 'window-configuration-change-hook 'maple-modeline-set-selected-window)
    (advice-remove 'handle-switch-frame 'maple-modeline-set-selected-window)
    (advice-remove 'select-frame 'maple-modeline-set-selected-window)
    (advice-remove 'load-theme 'maple-modeline--separator-reset))
  (setf (default-value 'mode-line-format) mode-line-format))

(provide 'maple-modeline)
;;; maple-modeline.el ends here
