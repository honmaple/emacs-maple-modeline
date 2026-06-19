;;; maple-modeline-separators.el --- Modeline separator configurations	-*- lexical-binding: t -*-

;; Copyright (C) 2018-2026 lin.jiang

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

(defcustom maple-modeline-separator 'arrow
  "Maple modeline separator style."
  :type '(choice (const default)
                 (const bar)
                 (const arrow)
                 (const slant)
                 (const slant-2x)
                 (const circle)
                 (const smootherstep)
                 (const cubic-bezier)
                 (const gradient)
                 (const icon-ice)
                 (const icon-flame)
                 (const icon-arrow)
                 (const icon-triangle)
                 (const icon-triangle-up)
                 (const icon-trapezoid)
                 (const icon-circle)
                 (const icon-pixelated)
                 (const icon-pixelated-sm)
                 (string)
                 (function))
  :group 'maple-modeline)

(defcustom maple-modeline-separator-alist
  '((default . maple-modeline-separator--default)
    (gradient . maple-modeline-separator--gradient))
  "Maple-modeline separator list."
  :type '(alist :key-type symbol :value-type function)
  :group 'maple-modeline)

(defcustom maple-modeline-separator-cache t
  "Whether enable separator cache."
  :type 'boolean
  :group 'maple-modeline)

(defvar maple-modeline--separator-cache nil)
(defvar maple-modeline--separator-chars
  (append (mapcar 'number-to-string (number-sequence 0 9))
          (mapcar 'char-to-string (number-sequence ?a ?z))))

(defun maple-modeline--separator-reset(&rest _)
  "Reset separator cache."
  (setq maple-modeline--separator-cache nil))

(defmacro maple-modeline-separator-with-cache(key &rest args)
  "Return separator with cache KEY, or execute ARGS."
  (declare (indent 1) (doc-string 2))
  `(if maple-modeline-separator-cache
       (let ((sep (cdr (assoc ,key maple-modeline--separator-cache))))
         (unless sep
           (setq sep ,@args)
           (push (cons ,key sep) maple-modeline--separator-cache))
         sep)
     ,@args))

(defun maple-modeline-separator-render(style height color0 color1 &optional reverse)
  "Draw COLOR0 COLOR1 &OPTIONAL REVERSE HEIGHT WIDTH STYLE."
  (when-let ((func (cdr (assq style maple-modeline-separator-alist))))
    (maple-modeline-separator-with-cache (list style height color0 color1 reverse)
      (funcall func height color0 color1 reverse))))

(defun maple-modeline--format-separator(separator color0 color1 &optional reverse)
  "SEPARATOR COLOR0 COLOR1 &OPTIONAL REVERSE."
  (let ((height (or maple-modeline-height (- (elt (window-pixel-edges) 3) (elt (window-inside-pixel-edges) 3))))
        (separator (or separator maple-modeline-separator)))
    (cond ((not separator) "")
          ((stringp separator) separator)
          ((functionp separator) (funcall separator height color0 color1 reverse))
          (t
           (maple-modeline-separator-render separator height color0 color1 reverse)))))

(defmacro maple-modeline-define-separator (name &rest args)
  "Define separator named NAME with ARGS."
  (declare (indent 1) (doc-string 2))
  (let* ((options nil)
         (_ (while (keywordp (car args))
              (push (pop args) options)
              (push (pop args) options)))
         (scale  (or (plist-get (nreverse options) :scale) 1))
         (-name (intern (format "%s" name)))
         (-func (intern (format "maple-modeline-separator--%s" name))))
    `(progn
       (add-to-list 'maple-modeline-separator-alist (cons ',-name ',-func))
       (defun ,-func (height color0 color1 &optional reverse)
         (when (display-graphic-p)
           (when reverse
             (setq color0 (prog1 color1 (setq color1 color0))))
           (let* ((scale ,scale)
                  (height (* height scale))
                  (result ,@args)
                  (image-props (list :ascent 'center)))
             (when (> scale 1)
               (setq image-props
                     (append image-props
                             (list :scale (/ 1.0 scale)
                                   :transform-smoothing t))))
             (propertize
              " " 'display
              (apply
               #'create-image
               (format "/* XPM */\nstatic char * %s[] = {\n\"%s %s 2 1\",\n\"0 c %s\",\n\"1 c %s\",\n%s};"
                       (concat (replace-regexp-in-string "-" "_" ,(format "%s" name)) "_" (if reverse "right" "left"))
                       (length (car result)) (length result)
                       (or color0 "None") (or color1 "None")
                       (cl-loop for rows in result
                                for row = (mapconcat (lambda (x) (format "%s" x)) rows "")
                                concat "\""
                                concat (if reverse (reverse row) row)
                                concat "\",\n"))
               'xpm t
               image-props))))))))

(maple-modeline-define-separator bar
  (cl-loop for y from 0 below height collect '(0 0)))

;; |y| = x - (height / 2)
(maple-modeline-define-separator arrow
  (let* ((height (if (cl-evenp height) height (- height 1)))
         (middle (/ height 2))
         (width  middle))
    (cl-loop for y from (- 0 middle) to middle
             collect (cl-loop for x from 0 to width
                              ;; |y| <= (height / 2) - x
                              if (<= (+ (abs y) x) middle)
                              collect 0 else collect 1))))

;; y = x/2
(maple-modeline-define-separator slant
  (let* ((width (/ height 2)))
    (cl-loop for y from 0 below height
             collect (cl-loop for x from 0 below width
                              if (<= x (/ y 2))
                              collect 0 else collect 1))))

;; y = x
(maple-modeline-define-separator slant-2x
  (let* ((width height))
    (cl-loop for y from 0 below height
             collect (cl-loop for x from 0 below width
                              if (<= x y)
                              collect 0 else collect 1))))

;; x^2 + y^2 = (height / 2)^2
(maple-modeline-define-separator circle
  :scale 2
  (let* ((height (if (cl-evenp height) height (- height 1)))
         (middle (/ height 2))
         (width  middle)
         (r-squared (* middle middle)))
    (cl-loop for y from (- 0 middle) to middle
             collect (cl-loop for x from 0 to width
                              ;; x^2 + y^2 <= (height / 2)^2
                              if (<= (+ (* x x) (* y y)) r-squared)
                              collect 0 else collect 1))))

;; Ken Perlin's quintic smootherstep: y = height * (6t^5 - 15t^4 + 10t^3)
(maple-modeline-define-separator smootherstep
  :scale 2
  (let* ((width (max 2 (/ height 2)))
         (height-max (1- height))
         (width-max (1- width)))
    (cl-loop for y from 0 below height
             collect (cl-loop for x from 0 below width
                              for progress = (/ (float x) width-max)
                              for boundary = (* height-max
                                                (+ (* 6 (expt progress 5))
                                                   (* -15 (expt progress 4))
                                                   (* 10 (expt progress 3))))
                              if (>= y boundary)
                              collect 0 else collect 1))))

(defun maple-modeline-separator--cubic-bezier-value (progress)
  "Return cubic-bezier(.5, 0, .5, 1) y value at PROGRESS."
  (let ((lower 0.0)
        (upper 1.0)
        (param 0.5))
    (dotimes (_ 8)
      (setq param (/ (+ lower upper) 2.0))
      (if (< (+ (* 1.5 param)
                (* -1.5 param param)
                (* param param param))
             progress)
          (setq lower param)
        (setq upper param)))
    (+ (* 3 param param)
       (* -2 param param param))))

;; CSS cubic-bezier(.5, 0, .5, 1)
(maple-modeline-define-separator cubic-bezier
  :scale 2
  (let* ((width (max 2 (/ height 2)))
         (height-max (1- height))
         (width-max (1- width)))
    (cl-loop for y from 0 below height
             collect (cl-loop for x from 0 below width
                              for progress = (/ (float x) width-max)
                              for boundary = (* height-max
                                                (maple-modeline-separator--cubic-bezier-value progress))
                              if (>= y boundary)
                              collect 0 else collect 1))))

(defun maple-modeline-separator--gradient (height color0 color1 &optional reverse)
  "COLOR0 COLOR1 &OPTIONAL REVERSE HEIGHT."
  (let* ((width  (min (/ height 2) (- (length maple-modeline--separator-chars) 1))))
    (propertize
     " " 'display
     (create-image
      (format "/* XPM */\nstatic char * gradient[] = {\n\"%s %s %s 1\",\n %s %s};"
              width height width
              (cl-loop for index from 0
                       for color in (color-gradient
                                     (color-name-to-rgb color0)
                                     (color-name-to-rgb color1) width)
                       concat (format "\"%s c %s\",\n" (nth index maple-modeline--separator-chars) (apply 'color-rgb-to-hex color)))
              (cl-loop for y from 0 below height
                       concat "\""
                       concat (cl-loop for x from 0 below width
                                       concat (nth x maple-modeline--separator-chars))
                       concat "\",\n"))
      'xpm t
      :ascent 'center))))

(defun maple-modeline-separator--default (_ color0 color1 &optional reverse)
  "COLOR0 COLOR1 &OPTIONAL REVERSE HEIGHT."
  (propertize
   (char-to-string (if reverse #xe0b2 #xe0b0))
   'face (list :background (if reverse color0 color1)
               :foreground (if reverse color1 color0))))

(defmacro maple-modeline-define-icon-separator (name &rest args)
  "Define icon with NAME ARGS."
  (declare (indent 1) (doc-string 2))
  (let ((-name (intern (format "icon-%s" name)))
        (-func (intern (format "maple-modeline-separator--icon-%s" name))))
    `(progn
       (add-to-list 'maple-modeline-separator-alist (cons ',-name ',-func))
       (defun ,-func (height color0 color1 &optional reverse)
         (when reverse
           (setq color0 (prog1 color1 (setq color1 color0))))
         (let* ((height (float height))
                (size   (or (ignore-errors (font-get (face-attribute 'default :font) :size)) 12))
                (icon   ,@args))
           (when icon
             (propertize icon 'face (list :inherit (get-text-property 0 'face icon)
                                          :height (/ height size)
                                          :foreground color0
                                          :background color1))))))))

(defun maple-modeline-separator--icon (icon font-size)
  "ICON FONT-SIZE."
  (when (fboundp 'nerd-icons-powerline)
    (nerd-icons-powerline icon :v-adjust (* (/ 1.0 font-size) -1.0))))

(maple-modeline-define-icon-separator ice
  (maple-modeline-separator--icon
   (if reverse "nf-ple-ice_waveform_mirrored" "nf-ple-ice_waveform") size))

(maple-modeline-define-icon-separator flame
  (maple-modeline-separator--icon
   (if reverse "nf-ple-flame_thick_mirrored" "nf-ple-flame_thick") size))

(maple-modeline-define-icon-separator arrow
  (maple-modeline-separator--icon
   (if reverse "nf-pl-right_hard_divider" "nf-pl-left_hard_divider") size))

(maple-modeline-define-icon-separator triangle
  (maple-modeline-separator--icon
   (if reverse "nf-ple-lower_right_triangle" "nf-ple-lower_left_triangle") size))

(maple-modeline-define-icon-separator triangle-up
  (maple-modeline-separator--icon
   (if reverse "nf-ple-upper_right_triangle" "nf-ple-upper_left_triangle") size))

(maple-modeline-define-icon-separator trapezoid
  (maple-modeline-separator--icon
   (if reverse "nf-ple-trapezoid_top_bottom_mirrored" "nf-ple-trapezoid_top_bottom") size))

(maple-modeline-define-icon-separator circle
  (maple-modeline-separator--icon
   (if reverse "nf-ple-left_half_circle_thick" "nf-ple-right_half_circle_thick") size))

(maple-modeline-define-icon-separator pixelated
  (maple-modeline-separator--icon
   (if reverse "nf-ple-pixelated_squares_big_mirrored" "nf-ple-pixelated_squares_big") size))

(maple-modeline-define-icon-separator pixelated-sm
  (maple-modeline-separator--icon
   (if reverse "nf-ple-pixelated_squares_small_mirrored" "nf-ple-pixelated_squares_small") size))

(provide 'maple-modeline-separators)
;;; maple-modeline-separators.el ends here
