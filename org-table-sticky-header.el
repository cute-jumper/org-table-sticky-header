;;; org-table-sticky-header.el --- Sticky header for org-mode tables  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Junpeng Qiu

;; Author: Junpeng Qiu <qjpchmail@gmail.com>
;; Keywords: extensions
;; Package-Requires: ((org "9.0.5"))

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

(require 'org)

(defvar org-table-sticky-header--last-win-start -1)
(defvar org-table-sticky-header--old-header-line-format nil)

(defun org-table-sticky-header-org-table-header-visible-p ()
  (save-excursion
    (goto-char org-table-sticky-header--last-win-start)
    (>= (org-table-begin) (point))))

(defun org-table-sticky-header-get-org-table-header ()
  (let ((col (window-hscroll)))
    (save-excursion
      (goto-char org-table-sticky-header--last-win-start)
      (if (bobp)
          ""
        (if (org-at-table-p 'any)
            (goto-char (org-table-begin))
          (forward-line -1))
        (buffer-substring-no-properties (+ (point-at-bol) col) (point-at-eol))))))

(defun org-table-sticky-header--fetch-header ()
  (if (org-table-sticky-header-org-table-header-visible-p)
      (setq header-line-format org-table-sticky-header--old-header-line-format)
    ;; stole from `semantic-stickyfunc-mode'
    (setq header-line-format
          '(:eval (list
                   (propertize " " 'display '((space :align-to 0)))
                   (org-table-sticky-header-get-org-table-header))))))

(defun org-table-sticky-header--scroll-function (win start-pos)
  (unless (= org-table-sticky-header--last-win-start start-pos)
    (setq org-table-sticky-header--last-win-start start-pos)
    (org-table-sticky-header--fetch-header)))

(define-minor-mode org-table-sticky-header-mode
  "Sticky header for org-mode tables."
  nil " OTSH" nil
  (if org-table-sticky-header-mode
      (if (derived-mode-p 'org-mode)
          (progn
            (setq org-table-sticky-header--old-header-line-format header-line-format)
            (add-hook 'window-scroll-functions
                      'org-table-sticky-header--scroll-function 'append 'local)
            (setq org-table-sticky-header--last-win-start (window-start))
            (org-table-sticky-header--fetch-header))
        (setq org-table-sticky-header-mode nil)
        (error "Not in `org-mode'"))
    (remove-hook 'window-scroll-functions 'org-table-sticky-header--scroll-function 'local)
    (setq header-line-format org-table-sticky-header--old-header-line-format)))

(provide 'org-table-sticky-header)
;;; org-table-sticky-header.el ends here
