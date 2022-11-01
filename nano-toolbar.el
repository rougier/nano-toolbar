;;; nano-toolbar.el --- Toolbar in the header line -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Nicolas P. Rougier

;; Maintainer: Nicolas P. Rougier <Nicolas.Rougier@inria.fr>
;; URL: https://github.com/rougier/nano-toolbar
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1" svg-lib))
;; Keywords: widgets, header)line

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;

;;; Code:
(require 'nano-theme)
(require 'svg-lib)

(defvar nano-toolbar-prompt-face 'nano-strong)

;; You might need to change these faces if you're not using nano-theme
(defvar nano-toolbar-button-faces '((0 . nano-default)     ;; Inactive
                                    (1 . nano-faded-i)     ;; Pre-selected
                                    (2 . nano-default-i))) ;; Active

(defvar nano-toolbar-space-top 0.20
  "Empty top space (purely aesthetic)")
  
(defvar nano-toolbar-space-bottom  0.25
  "Empty bottom space (purely aesthetic)")

(defvar nano-toolbar--buttons nil
  "List of buttons as a cons cell (LABEL . STATE)")

(defvar nano-toolbar--index-current nil
  "Index of the current selected button")

(defvar nano-toolbar--index-pointer nil
    "Index of the current mouse selected button")


(defun nano-toolbar--make-button (button)
  "Create a BUTTON with given label and state and install mouse handlers."

  (let* ((label (car button))
         (state (cdr button))
         (face (cdr (assoc state nano-toolbar-button-faces)))
         (color-luminance-dark-limit 0.5)
         (dark (color-dark-p (color-name-to-rgb (face-background face nil 'default))))
         (stroke (if dark 0 2))
         (weight (if dark 'semibold 'light))
         (foreground (face-foreground face nil 'default))
         (background (face-background face nil 'default)))
    (propertize label
                'pointer 'hand
                'help-echo `(lambda (window object pos)
                              (nano-toolbar--highlight-button ,label))
                'keymap (let ((map (make-sparse-keymap)))
                        (define-key map [header-line mouse-1]
                          `(lambda () (interactive) (nano-toolbar--activate-button ,label)))
                        map)
                'display (svg-lib-tag label nil :stroke stroke
                                                :font-weight weight
                                                :foreground foreground
                                                :background background))))

(defun nano-toolbar--highlight-button (label)
  "Change button LABEL to highlight state."

  (setq nano-toolbar--index-pointer nil)
  (let ((index 0))
    (dolist (button nano-toolbar--buttons)
      (when (not (equal (cdr button) 2))
        (if (equal (car button) label)
            (progn
              (setcdr button 1)
              (setq nano-toolbar--index-pointer index))
          (setcdr button 0)))
      (setq index (+ index 1))))
    (force-mode-line-update))


(defun nano-toolbar--activate-button (label)
  "Change button LABEL to activated state."

  (dolist (button nano-toolbar--buttons)
    (if (equal (car button) label)
        (setcdr button 2)
      (setcdr button 0)))
  (force-mode-line-update))

(defun nano-toolbar--mouse-activate ()
  "Activate current mouse selection."
  
  (interactive)
  (when nano-toolbar--index-pointer
    (setq nano-toolbar--index-current nano-toolbar--index-pointer)
    (let ((label (car (nth nano-toolbar--index-current nano-toolbar--buttons))))
      (nano-toolbar--activate-button label))
    (setq nano-toolbar--index-pointer nil)
    (throw 'break nil)))

(defun nano-toolbar--activate-next ()
  "Activate next button"
  
  (interactive)
  (setq nano-toolbar--index-current (mod (+ nano-toolbar--index-current 1) (length nano-toolbar--buttons)))
  (let ((label (car (nth nano-toolbar--index-current nano-toolbar--buttons))))
    (nano-toolbar--activate-button label)))

(defun nano-toolbar--activate-prev ()
  "Activate previous button"
  
  (interactive)
  (setq nano-toolbar--index-current (mod (- nano-toolbar--index-current 1) (length nano-toolbar--buttons)))
  (let ((label (car (nth nano-toolbar--index-current nano-toolbar--buttons))))
    (nano-toolbar--activate-button label)))

(defun nano-toolbar--select ()
  "Select current button"
  
  (interactive)
  (let ((label (car (nth nano-toolbar--index-current nano-toolbar--buttons))))
    (nano-toolbar--activate-button label)))

(defun nano-toolbar--cancel ()
  "Cancel toolbar"
  (throw 'break nil))

(defun nano-toolbar--validate ()
  "Validate current button"
  (throw 'break nil))

(defun nano-toolbar-make (prompt buttons)
  "Create a toolbar in the header line using given PROMPT and BUTTONS description"
  
  (setq nano-toolbar--buttons buttons)
  (setq nano-toolbar--index-current 0)
  (setq nano-toolbar--index-mouse nil)
  (setq saved-header-line-format header-line-format)
  
  ;; We use tooltip mode to highlight button under mouse cursor
  (tooltip-mode t)
  (setq tooltip-delay 0.0)
  (advice-add 'tooltip-hide :before
              (lambda (&optional args)
                (nano-toolbar--highlight-button "")))

  ;; Install toolbar 
  (setq-local header-line-format
            `(:eval
              (concat
               (propertize ,prompt 'face nano-toolbar-prompt-face
                           'display `(raise 0.1))
               (if (> (length ,prompt) 0) " " "")
               (mapconcat (lambda (button) (nano-toolbar--make-button button)) nano-toolbar--buttons " ")
               (propertize " " 'face `(:underline ,(face-background 'default))
                               'display `(raise ,nano-toolbar-space-top))
               (propertize " " 'face `(:underline ,(face-background 'default))
                           'display `(raise ,(- nano-toolbar-space-bottom))))))

  ;; Process keyboard and mouse events (blocking)
  (catch 'break
    (while t
      (let* ((event (read-key-sequence nil))
             (key (key-description event)))
        (cond ((string= key "C-g")     (nano-toolbar--cancel))
              ((string= key "RET")     (nano-toolbar--validate))
              ((string= key "SPC")     (nano-toolbar--select))
              ((string= "<header-line> <mouse-1>" (nano-toolbar--mouse-activate)))
              ((string= key "<right>") (nano-toolbar--activate-next))
              ((string= key "<left>")  (nano-toolbar--activate-prev))))))

  ;; Restore header-line
  (setq-local header-line-format saved-header-line-format)

  ;; Read selection
  (catch 'break
    (dolist (button nano-toolbar--buttons)
      (if (equal (cdr button) 2)
          (throw 'break (car button))))))



(nano-toolbar-make " SELECT" '(("UNREAD"    . 2)
                               ("TODO"      . 0)
                               ("INBOX"     . 0)
                               ("TODAY"     . 0)
                               ("FLAGGED"   . 0)
                               ("YESTERDAY" . 0)
                               ("LAST WEEK" . 0)))
