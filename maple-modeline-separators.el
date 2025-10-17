;;; maple-modeline-separators.el --- create xpm image configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2018-2019 lin.jiang

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
;; modeline separator.
;;

;;; Code:
(require 'color)
(require 'maple-modeline-core)

(defcustom maple-modeline-separator 'default
  "Maple modeline separator style."
  :type '(choice (const default)
                 (const wave)
                 (const line)
                 (const bar)
                 (const slant)
                 (const contour)
                 (const box)
                 (const butt)
                 (const curve)
                 (const zigzag)
                 (const gradient))
  :group 'maple-modeline)

(defcustom maple-modeline-separator-alist
  '((nil . maple-modeline-separator--nil)
    (default . maple-modeline-separator--default)
    (gradient . maple-modeline-separator--gradient))
  "Maple-modeline separator list."
  :type '(alist :key-type symbol :value-type function)
  :group 'maple-modeline)

(defvar maple-modeline--separator-cache nil)
(defvar maple-modeline--separator-chars
  (append (mapcar 'number-to-string (number-sequence 0 9))
          (mapcar 'char-to-string (number-sequence ?a ?z))))

(defun maple-modeline--separator-color (face)
  "Covert COLOR with FACE."
  (let ((color (maple-modeline--separator-background face)))
    (if (and (eq system-type 'darwin) (not (boundp 'mac-carbon-version-string)))
        (pcase-let*
            ((`(,r ,g ,b) (color-name-to-rgb color))
             (`(,x ,y ,z) (color-srgb-to-xyz r g b))
             (r (expt (+ (* 3.2404542 x) (* -1.5371385 y) (* -0.4985314 z))
                      (/ 1.8)))
             (g (expt (+ (* -0.9692660 x) (* 1.8760108 y) (* 0.0415560 z))
                      (/ 1.8)))
             (b (expt (+ (* 0.0556434 x) (* -0.2040259 y) (* 1.0572252 z))
                      (/ 1.8))))
          (color-rgb-to-hex r g b))
      (apply 'color-rgb-to-hex (color-name-to-rgb color)))))

(defun maple-modeline--separator-background (face)
  "Get FACE background."
  (let ((background (if (listp face) (plist-get face :background )
                      (face-attribute face :background nil t))))
    (if (or (not background)
            (eq background 'unspecified))
        (face-attribute 'default :background)
      background)))

(defun maple-modeline--separator-height ()
  "Get default height."
  (or maple-modeline-height (frame-char-height)))

(defun maple-modeline--separator-string(pattern)
  "To string with PATTERN."
  (concat "\"" (mapconcat (lambda(x) (format "%s" x)) pattern "") "\","))

(defun maple-modeline--separator-reset(&rest _)
  "Reset separator cache."
  (setq maple-modeline--separator-cache nil))

(defun maple-modeline-separator-draw(style face1 face2 &optional reverse height width)
  "Draw FACE1 FACE2 &OPTIONAL REVERSE HEIGHT WIDTH STYLE."
  (let* ((func (cdr (assq style maple-modeline-separator-alist)))
         (key (list style face1 face2 height reverse)))
    (when func
      (or (cdr (assoc key maple-modeline--separator-cache))
          (let ((image (propertize " " 'display (funcall func face1 face2 reverse height width))))
            (push (cons key image) maple-modeline--separator-cache) image)))))

(defmacro maple-modeline-define-separator (name center &optional header footer)
  "NAME CENTER &OPTIONAL HEADER FOOTER."
  (declare (indent 1) (doc-string 2))
  (let* ((-name (format "%s" name))
         (-func (intern (format "maple-modeline-separator--%s" -name))))
    `(progn
       (add-to-list 'maple-modeline-separator-alist (cons ',name ',-func))
       (defun ,-func (face1 face2 &optional reverse height width)
         (when (display-graphic-p)
           (when reverse (setq face1 (prog1 face2 (setq face2 face1))))
           (let* ((name (replace-regexp-in-string "-" "_" ,-name))
                  (color1 (or (maple-modeline--separator-color face1) "None"))
                  (color2 (or (maple-modeline--separator-color face2) "None"))
                  (color3 color1)
                  (height (or height (maple-modeline--separator-height)))
                  (width (or width (length (or (car ,center) (car ,header) (car ,footer)))))
                  (dir (if reverse "right" "left"))
                  (header-pattern (mapcar 'maple-modeline--separator-string
                                          (if reverse (mapcar 'reverse ,header) ,header)))
                  (footer-pattern (mapcar 'maple-modeline--separator-string
                                          (if reverse (mapcar 'reverse ,footer) ,footer)))
                  (pattern (mapcar 'maple-modeline--separator-string
                                   (if reverse (mapcar 'reverse ,center) ,center)))
                  (pattern-height (max (- height (+ (length ,header) (length ,footer))) 0)))
             (create-image
              (format "/* XPM */ static char * %s_%s[] = {
                                         \"%s %s 3 1\",
                                         \"0 c %s\",
                                         \"1 c %s\",
                                         \"2 c %s\",
                                         %s};"
                      name dir width height color1 color2 color3
                      (concat (when ,header
                                (mapconcat 'identity header-pattern ""))
                              (when ,center
                                (mapconcat 'identity (make-list pattern-height (mapconcat 'identity pattern "")) ""))
                              (when ,footer
                                (mapconcat 'identity footer-pattern ""))))
              'xpm t
              :ascent 'center)))))))


(maple-modeline-define-separator line
  '((2)))

(maple-modeline-define-separator bar
  '((2 2)))

(maple-modeline-define-separator wave
  '((0 0 0 0 0 0 1 1 1 1 1))
  '((2 1 1 1 1 1 1 1 1 1 1)
    (0 0 1 1 1 1 1 1 1 1 1)
    (0 0 0 1 1 1 1 1 1 1 1)
    (0 0 0 2 1 1 1 1 1 1 1)
    (0 0 0 0 1 1 1 1 1 1 1)
    (0 0 0 0 2 1 1 1 1 1 1)
    (0 0 0 0 0 1 1 1 1 1 1)
    (0 0 0 0 0 1 1 1 1 1 1)
    (0 0 0 0 0 2 1 1 1 1 1))
  '((0 0 0 0 0 0 2 1 1 1 1)
    (0 0 0 0 0 0 0 1 1 1 1)
    (0 0 0 0 0 0 0 1 1 1 1)
    (0 0 0 0 0 0 0 2 1 1 1)
    (0 0 0 0 0 0 0 0 1 1 1)
    (0 0 0 0 0 0 0 0 2 1 1)
    (0 0 0 0 0 0 0 0 0 0 2)))

(maple-modeline-define-separator contour
  '((0 0 0 0 0 1 1 1 1 1))
  '((1 1 1 1 1 1 1 1 1 1)
    (0 2 1 1 1 1 1 1 1 1)
    (0 0 2 1 1 1 1 1 1 1)
    (0 0 0 2 1 1 1 1 1 1)
    (0 0 0 0 1 1 1 1 1 1)
    (0 0 0 0 2 1 1 1 1 1))
  '((0 0 0 0 0 2 1 1 1 1)
    (0 0 0 0 0 0 1 1 1 1)
    (0 0 0 0 0 0 2 1 1 1)
    (0 0 0 0 0 0 0 2 1 1)
    (0 0 0 0 0 0 0 0 0 0)))

(maple-modeline-define-separator butt
  '((0 0 0))
  '((1 1 1)
    (0 1 1)
    (0 0 1))
  '((0 0 1)
    (0 1 1)
    (1 1 1)))

(maple-modeline-define-separator box
  '((0 0)
    (0 0)
    (1 1)
    (1 1)))

(maple-modeline-define-separator curve
  '((0 0 0 0))
  '((1 1 1 1)
    (2 1 1 1)
    (0 0 1 1)
    (0 0 2 1)
    (0 0 0 1)
    (0 0 0 2))
  '((0 0 0 2)
    (0 0 0 1)
    (0 0 2 1)
    (0 0 1 1)
    (2 1 1 1)
    (1 1 1 1)))

(maple-modeline-define-separator zigzag
  '((1 1 1)
    (0 1 1)
    (0 0 1)
    (0 0 0)
    (0 0 1)
    (0 1 1)))

(maple-modeline-define-separator slant
  nil
  (cl-loop
   for i from 1 to height collect
   (let ((x (/ i 2)))
     (append (make-list x 0)
             (make-list 1 2)
             (make-list (max 0 (- 10 x)) 1)))))

(defun maple-modeline-separator--gradient (face1 face2 &optional reverse height width)
  "FACE1 FACE2 &OPTIONAL REVERSE HEIGHT WIDTH."
  (ignore reverse)
  (let* ((color1 (maple-modeline--separator-background face1))
         (color2 (maple-modeline--separator-background face2))
         (height (or height (maple-modeline--separator-height)))
         (width  (or width 13))
         (number -1))
    (create-image
     (format "/* XPM */ static char * gradient[] = {\"%s %s %s 1\", %s %s};"
             width height width
             (mapconcat
              (lambda(x)
                (setq number (+ number 1))
                (format "\"%s c %s\"," (nth number maple-modeline--separator-chars) (apply 'color-rgb-to-hex x)))
              (color-gradient
               (color-name-to-rgb color1)
               (color-name-to-rgb color2) width) "")
             (mapconcat
              'identity
              (make-list height (maple-modeline--separator-string
                                 (cl-subseq maple-modeline--separator-chars 0 (min width (length maple-modeline--separator-chars))))) ""))
     'xpm t
     :ascent 'center)))

(defun maple-modeline-separator--default (face1 face2 &optional reverse height width)
  "FACE1 FACE2 &OPTIONAL REVERSE HEIGHT WIDTH."
  (ignore height) (ignore width)
  (propertize
   (char-to-string (if reverse #xe0b2 #xe0b0))
   'face (list :background (maple-modeline--separator-background (if reverse face1 face2))
               :foreground (maple-modeline--separator-background (if reverse face2 face1)))))

(defun maple-modeline-separator--nil (face1 face2 &optional reverse height width)
  "FACE1 FACE2 &OPTIONAL REVERSE HEIGHT WIDTH."
  (ignore face1 face2 reverse height width)
  "")

(advice-add 'load-theme :after #'maple-modeline--separator-reset)

(provide 'maple-modeline-separators)
;;; maple-modeline-separators.el ends here