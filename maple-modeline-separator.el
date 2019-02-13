;;; maple-modeline-separator.el --- modeline configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2015-2019 lin.jiang

;; Author: lin.jiang <mail@honmaple.com>
;; URL: https://github.com/honmaple/dotfiles/tree/master/emacs.d

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
;; modeline separator configurations.
;;

;;; Code:
(defvar maple-modeline--cache nil)

(defcustom maple-modeline-sep 'default
  "Style."
  :group 'maple-modeline
  :type '(choice (const default)
                 (const wave)
                 (const circle)))

(defcustom maple-modeline-height (- (elt (window-pixel-edges) 3)
                                    (elt (window-inside-pixel-edges) 3))
  "Height of the mode line in pixels.
This should be an even number."
  :type 'integer
  :group 'maple-modeline)

(defun maple-modeline-draw(face0 face1 &optional reverse)
  "Draw FACE0 FACE1 HEIGHT REVERSE."
  (unless (cl-evenp maple-modeline-height)
    (cl-incf maple-modeline-height))
  (propertize
   " " 'display
   (let* ((color0 (or (face-background face0 nil t) "None"))
          (color1 (or (face-background face1 nil t) "None"))
          (height maple-modeline-height)
          (width (/ height 2))
          key)
     (when reverse (setq color0 (prog1 color1 (setq color1 color0))))
     (setq key (list color0 color1 height reverse))
     (or (cdr (assoc key maple-modeline--cache))
         (let ((image (create-image
                       (format
                        "/* XPM */ static char * image[] = {
        \"%s %s 3 1\",
        \". c %s\",
        \"1 c %s\",
        \"* c %s\",
        %s};"
                        width height color0 color0 color1
                        (cond ((eq maple-modeline-sep 'wave)
                               (maple-modeline-separator--wave height width reverse))
                              ((eq maple-modeline-sep 'circle)
                               (maple-modeline-separator--circle height width reverse))
                              (t (maple-modeline-separator--default height width reverse))))
                       'xpm t :ascent 'center)))
           (push (cons key image) maple-modeline--cache)
           image)))))

(defun maple-modeline-separator--default(height width &optional reverse)
  "Draw HEIGHT WIDTH REVERSE."
  (cl-loop
   for i from 1 to height concat
   (format "\"%s\",\n"
           (let* ((x (/ i 2))
                  (a (make-string x ?.))
                  (b (make-string 1 ?1))
                  (c (make-string
                      (max 0 (- width x)) ?*)))
             (if reverse
                 (concat c b a)
               (concat a b c))))))

(defun maple-modeline-separator--wave(height width &optional reverse)
  "Draw HEIGHT WIDTH REVERSE."
  (cl-loop
   for i from 1 to height concat
   (format "\"%s\",\n"
           (let* ((v (tan (asin (/ i (float height)))))
                  (x (if (= v 0) width (ceiling (/ 1 v))))
                  (a (make-string (min width x) ?.))
                  (b (make-string 1 ?1))
                  (c (make-string (max 0 (- width x)) ?*)))
             (if reverse
                 (concat c b a)
               (concat a b c))))))

(defun maple-modeline-separator--circle(height width &optional reverse)
  "Draw HEIGHT WIDTH REVERSE."
  (cl-loop
   for i from 1 to height concat
   (format "\"%s\",\n"
           (let* ((x (round
                      (if (> i width) (- height (sqrt (- (* height i) (* i i))))
                        (sqrt (- (* height i) (* i i))))))
                  (a (make-string x ?.))
                  (b (make-string 1 ?1))
                  (c (make-string (max 0 (- height x)) ?*)))
             (if reverse
                 (concat c b a)
               (concat a b c))))))

(provide 'maple-modeline-separator)
;;; maple-modeline-separator.el ends here
