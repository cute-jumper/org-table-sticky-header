;;; org-table-sticky-header.el --- Sticky header for org-mode tables  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Junpeng Qiu

;; Author: Junpeng Qiu <qjpchmail@gmail.com>
;; Keywords: extensions

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(defvar otsh--last-win-start -1)
(defvar otsh--old-header-line-format nil)

(defun otsh-org-table-header-visible-p ()
  (save-excursion
    (goto-char otsh--last-win-start)
    (>= (org-table-begin) (point))))

(defun otsh-get-org-table-header ()
  (let ((col (window-hscroll)))
    (save-excursion
      (goto-char otsh--last-win-start)
      (if (bobp)
          ""
        (if (org-at-table-p 'any)
            (goto-char (org-table-begin))
          (forward-line -1))
        (buffer-substring-no-properties (+ (point-at-bol) col) (point-at-eol))))))

(defun otsh--fetch-header ()
  (if (otsh-org-table-header-visible-p)
      (setq header-line-format otsh--old-header-line-format)
    ;; stole from `semantic-stickyfunc-mode'
    (setq header-line-format
          '(:eval (list
                   (propertize " " 'display '((space :align-to 0)))
                   (otsh-get-org-table-header))))))

(defun otsh--scroll-function (win start-pos)
  (unless (= otsh--last-win-start start-pos)
    (setq otsh--last-win-start start-pos)
    (otsh--fetch-header)))

(define-minor-mode org-table-sticky-header-mode
  "Sticky header for org-mode tables."
  nil " OTSH" nil
  (if org-table-sticky-header-mode
      (if (not (derived-mode-p 'org-mode))
          (error "Not in `org-mode'")
        (setq otsh--old-header-line-format header-line-format)
        (add-hook 'window-scroll-functions
                  'otsh--scroll-function 'append 'local)
        (setq otsh--last-win-start (window-start))
        (otsh--fetch-header))
    (remove-hook 'window-scroll-functions 'otsh--scroll-function 'local)
    (setq header-line-format otsh--old-header-line-format)))

(provide 'org-table-sticky-header)
;;; org-table-sticky-header.el ends here
