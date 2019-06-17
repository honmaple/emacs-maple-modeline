;;; maple-modeline-icon.el --- modeline configurations.	-*- lexical-binding: t -*-

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
;; modeline icon configurations.
;;

;;; Code:
(require 'all-the-icons)

(defun maple-modeline-icon(name face)
  "Draw icon with NAME and FACE."
  (all-the-icons-octicon name :v-adjust -0.05 :face face))

(defmacro maple-modeline-icon-define (name &rest args)
  "Define modeline with NAME and ARGS."
  (declare (indent 1) (doc-string 2))
  (let ((-name (format "%s" name))
        (-format (plist-get args :format)))
    `(defun ,(intern (format "maple-modeline--%s" -name)) (&optional face)
       (when (,(intern (format "maple-modeline-%s-show-p" -name)))
         (maple-modeline-raw ,-format face)))))

(maple-modeline-icon-define version-control
  :format
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

(provide 'maple-modeline-icon)
;;; maple-modeline-icon.el ends here
