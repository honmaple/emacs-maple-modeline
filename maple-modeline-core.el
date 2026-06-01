;;; maple-modeline-core.el --- Modeline core configurations	-*- lexical-binding: t -*-

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
(require 'cl-lib)
(require 'subr-x)
(require 'color)

(defgroup maple-modeline nil
  "Maple-modeline, a prettier mode line."
  :group 'maple)

(defcustom maple-modeline-style 'standard
  "Maple-modeline Style."
  :type '(choice (const standard)
                 (const minimal)
                 (const sidebar)
                 (const evil))
  :group 'maple-modeline)

(defcustom maple-modeline-side-style 'sidebar
  "Maple-modeline auto set side window style."
  :type '(choice (const standard)
                 (const minimal)
                 (const sidebar))
  :group 'maple-modeline)

(defcustom maple-modeline-style-alist nil
  "Maple-modeline style list."
  :type '(alist :key-type symbol :value-type list)
  :group 'maple-modeline)

(defcustom maple-modeline-width 'auto
  "The width of mode-line."
  :type '(choice (const standard)
                 (const auto))
  :group 'maple-modeline)

(defcustom maple-modeline-height (- (elt (window-pixel-edges) 3) (elt (window-inside-pixel-edges) 3) -4)
  "The height of mode-line."
  :type 'integer
  :group 'maple-modeline)

(defcustom maple-modeline-icon (display-graphic-p)
  "Whether display the icons in the mode-line."
  :type 'boolean
  :group 'maple-modeline)

(defcustom maple-modeline-direction
  (if (display-graphic-p) '(auto . auto) '(right . left))
  "The separator's direction of left-segments or right-segments."
  :type '(alist :key-type (choice (const auto)
                                  (const left)
                                  (const right))
                :value-type (choice (const auto)
                                    (const left)
                                    (const right)))
  :group 'maple-modeline)

(defcustom maple-modeline-face-alist
  '((evil . maple-modeline--evil-face)
    (window-number . maple-modeline--evil-face)
    (remote-host . mode-line-buffer-id)
    (projectile . mode-line-buffer-id)
    (region . mode-line-emphasis)
    (macro . mode-line-buffer-id)
    (iedit . mode-line-buffer-id))
  "Maple-modeline face list."
  :type '(alist :key-type symbol :value-type face)
  :group 'maple-modeline)

(defface maple-modeline-active0
  '((t (:inherit mode-line-active)))
  "Maple-modeline active face 0."
  :group 'maple-modeline)

(defun maple-modeline--background-lighten(face percent)
  "Lighten FACE's background with PERCENT."
  (let ((value (face-attribute face :background nil t)))
    (if (and value (not (eq value 'unspecified)))
        (color-lighten-name value percent)
      'unspecified)))

(defface maple-modeline-active1
  `((t (:inherit mode-line-active :background ,(maple-modeline--background-lighten 'mode-line-active 25))))
  "Maple-modeline active face 1."
  :group 'maple-modeline)

(defface maple-modeline-inactive0
  '((t (:inherit mode-line-inactive)))
  "Maple-modeline inactive face 0."
  :group 'maple-modeline)

(defface maple-modeline-inactive1
  `((t (:inherit mode-line-inactive :background ,(maple-modeline--background-lighten 'mode-line-inactive 25))))
  "Maple-modeline inactive face 1."
  :group 'maple-modeline)

(defun maple-modeline--evil-color()
  "The color of `evil-state` or cursor."
  (or (and (boundp 'evil-state)
           (pcase evil-state
             ('normal "DarkGoldenrod2")
             ('insert "chartreuse3")
             ('visual "gray")
             ('emacs "SkyBlue2")
             ('motion "plum3")
             ('replace "chocolate")
             (_ nil)))
      (face-attribute 'cursor :background)))

(defun maple-modeline--evil-face(face)
  "The FACE that using evil color as foreground."
  (let ((foreground (maple-modeline--evil-color))
        (background (face-attribute face :background nil t)))
    `(:inherit ,face :foreground ,foreground :background ,background)))

(defun maple-modeline--evil-background-face(face)
  "The FACE that using evil color as background."
  (let ((background (maple-modeline--evil-color))
        (foreground (face-attribute face :background nil t)))
    `(:inherit ,face :foreground ,foreground :background ,background)))

(defun maple-modeline--face(segment &optional default-face)
  "Get SEGMENT's face, when nil use DEFAULT-FACE."
  (let* ((name (if (listp segment) (car segment) segment))
         (face (cdr (assq name maple-modeline-face-alist))))
    (cond ((and face (facep face))
           (let ((background (face-attribute face :background nil t)))
             (if (and background (not (eq background 'unspecified))) face
               (list :inherit face :background (face-attribute default-face :background nil t)))))
          ((and face (listp face))
           (let ((background (plist-get face :background)))
             (if (and background (not (eq background 'unspecified))) face
               (append face (list :background (face-attribute default-face :background nil t))))))
          ((and face (functionp face))
           (funcall face default-face))
          (t default-face))))

(defun maple-modeline--color-hex (face attribute)
  "Get color hex from FACE's ATTRIBUTE."
  (let ((value (if (listp face) (plist-get face attribute)
                 (face-attribute face attribute nil t))))
    (unless (and value (not (eq value 'unspecified)))
      (setq value (face-attribute 'default attribute)))
    (let ((rgb (color-name-to-rgb value)))
      (if rgb (apply 'color-rgb-to-hex rgb) nil))))

(defun maple-modeline--foreground (face)
  "Get FACE foreground color."
  (maple-modeline--color-hex face :foreground))

(defun maple-modeline--background (face)
  "Get FACE background color."
  (maple-modeline--color-hex face :background))

(defun maple-modeline--string-pixel-width (str)
  "Return the width of STR in pixels."
  (if (fboundp 'string-pixel-width)
      (string-pixel-width str)
    (* (string-width str) (window-font-width nil 'mode-line)
       (if (display-graphic-p) 1.05 1.0))))

(defun maple-modeline--is-empty(str &optional trim)
  "Check STR TRIM is empty."
  (cond ((stringp str)
         (string= (if trim (string-trim str) str) ""))
        ((not str) t)))

(defun maple-modeline--plist (plist prop)
  "Get the values associated PLIST to PROP, a modified plist."
  (let ((tail plist)
        common
        result)
    (while (and (consp tail) (not (keywordp (car tail))))
      (when (not common) (setq common (list nil)))
      (push (pop tail) common))

    (while (and (consp tail) (not (eq prop (car tail))))
      (pop tail))
    ;; pop the found keyword
    (pop tail)
    (while (and (consp tail) (not (keywordp (car tail))))
      (push (pop tail) result))
    (append (cl-remove-if nil (append (nreverse common) (nreverse result)))
            (when tail (maple-modeline--plist tail prop)))))

(defmacro maple-modeline-define (name &rest args)
  "Set modeline with NAME and ARGS."
  (declare (indent 1) (doc-string 2))
  (let* ((left (or (plist-get args :left) '()))
         (right (or (plist-get args :right) '())))
    `(add-to-list 'maple-modeline-style-alist '(,name . (:left ,@left :right ,@right :separator ,(plist-get args :separator))))))

(provide 'maple-modeline-core)
;;; maple-modeline-core.el ends here