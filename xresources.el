;;; xresources.el --- Xresources -*- lexical-binding: t -*-
;;; Commentary:
;;; Module for writing .Xresources properties
;;; Code:

;(face-attribute 'border :background)

;(face-attribute 'default :font)

(defun xresources-color-name (color)
  "Return rgb hex representation of COLOR."

  (let ((decimal (mapcar (lambda (x) (/ x 256)) (color-values color))))
    (print decimal)
    (format "#%02x%02x%02x"
            (nth 0 decimal)
            (nth 1 decimal)
            (nth 2 decimal))))

(defalias 'xresources-color-face 'face-attribute)

(defvar xresources-default (list
                            (cons "*.foreground" (xresources-color-face 'default :foreground))
                            (cons "*.background" (xresources-color-face 'default :background))
                            (cons "*.cursorColor" (xresources-color-face 'cursor :background))
                            ;;; black
                            (cons "*.color0" (xresources-color-name "black"))
                            (cons "*.color8" (xresources-color-name "dim gray"))
                            ;;; red
                            (cons "*.color1" (xresources-color-name "red4"))
                            (cons "*.color9" (xresources-color-name "red1"))
                            ;;; green
                            (cons "*.color2" (xresources-color-name "green4"))
                            (cons "*.color10" (xresources-color-name "green1"))
                            ;;; yellow
                            (cons "*.color3" (xresources-color-name "yellow4"))
                            (cons "*.color11" (xresources-color-name "yellow1"))
                             ;;; blue
                            (cons "*.color4" (xresources-color-name "blue4"))
                            (cons "*.color12" (xresources-color-name "blue1"))
                            ;;; magenta
                            (cons "*.color5" (xresources-color-name "magenta4"))
                            (cons "*.color13" (xresources-color-name "magenta1"))
                            ;;; cyan
                            (cons "*.color6" (xresources-color-name "cyan4"))
                            (cons "*.color14" (xresources-color-name "cyan1"))
                            ;;; white
                            (cons "*.color7" (xresources-color-name "white smoke"))
                            (cons "*.color15" (xresources-color-name "white"))
                            ))

(defun hash-keys (hash)
  "Return keys of HASH."

  (let ((return (list)))
    (maphash (lambda (k v)
               (setq return (cons k return)))
             hash)
    return))

(defun xresources-table-to-hash (table)
  "Return a hash representation of the xresources table TABLE."

  (let ((hash (make-hash-table)))
    (dolist (element table)
      (puthash (car element) (cdr element) hash))
    hash
  ))

(defun xresources-output (&optional faces order)
  "Return .Xresources formatted string of FACES.
If ORDER is specified the properties are sorted."

  (let ((hash (xresources-table-to-hash (or faces xresources-default))))
    (mapconcat (lambda (x) (concat x ": " (gethash x hash)))
               (hash-keys hash)
               "\n")))

(defun xresources-write (&optional faces file)
  "Write .Xresources formatted string of FACES into FILE."

  (interactive)
  (with-temp-file (or file "~/.Xresources.faces")
    (insert (xresources-output faces))))

(provide 'xresources)

;;; xresources.el ends here
