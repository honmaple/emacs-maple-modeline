;;; maple-modeline-window.el --- modeline configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2015-2019 lin.jiang

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
;; modeline window configurations.
;;

;;; Code:
(defvar maple-modeline-priority-table (make-hash-table :test 'equal))
(defvar maple-modeline-selected-window (frame-selected-window))

(defun maple-modeline-set-selected-window (&rest _)
  "Set the variable `maple-modeline-selected-window` appropriately."
  (unless (minibuffer-window-active-p (frame-selected-window))
    (setq maple-modeline-selected-window (frame-selected-window))
    (force-mode-line-update)))

(defun maple-modeline-unset-selected-window ()
  "Unsets the variable `maple-modeline-selected-window` and update the modeline."
  (setq maple-modeline-selected-window nil)
  (force-mode-line-update))

(add-hook 'window-configuration-change-hook 'maple-modeline-set-selected-window)
(add-hook 'focus-in-hook 'maple-modeline-set-selected-window)
(add-hook 'focus-out-hook 'maple-modeline-unset-selected-window)

(advice-add 'handle-switch-frame :after 'maple-modeline-set-selected-window)
(advice-add 'select-frame  :after 'maple-modeline-set-selected-window)

(defun maple-modeline--active ()
  "Whether is an active window."
  (eq (selected-window) maple-modeline-selected-window))


(defun maple-modeline--table (hash-table)
  "Return a list of keys in HASH-TABLE."
  (let ((l '()))
    (maphash (lambda (k v) (push (cons k v) l)) hash-table)
    (sort l (lambda(a b) (< (cdr a) (cdr b))))))

(defun maple-modeline--reset()
  "Auto reset modeline width."
  (let* ((p (car (maple-modeline--table maple-modeline-priority-table)))
         (key (car p))
         (value (cdr p)))
    (puthash key (+ value 100) maple-modeline-priority-table)))

(defun maple-modeline-reset (f &optional width)
  "Setup modeline F WIDTH."
  (let* ((display (funcall `,f))
         (width (or width (with-current-buffer (current-buffer)
                            (+ (window-width)
                               (or (cdr (window-margins)) 0)
                               (or (car (window-margins)) 0)))))
         (modeline-width  (string-width display)))
    (while (> modeline-width width)
      (maple-modeline--reset)
      (setq display (funcall `,f))
      (setq modeline-width (string-width display)))
    (maphash
     (lambda (key value)
       (when (> value 100)
         (puthash key (- value 100) maple-modeline-priority-table)))
     maple-modeline-priority-table)
    display))

(provide 'maple-modeline-window)
;;; maple-modeline-window.el ends here
