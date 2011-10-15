;;; qsas.el --- QuickSilver's Abbreviation Scoring

;; Copyright (C) 2011  Tomohiro Matsuyama

;; Author: Tomohiro Matsuyama <tomo@cx4a.org>
;; Keywords: convenience

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

(eval-when-compile (require 'cl))

(defun qsas-count-matches-in-string (regexp string &optional start end)
  (setq start (or start 0)
        end   (or end (length string)))
  (loop for start = start then (1+ matched)
        for matched = (let ((case-fold-search nil))
                        (string-match regexp string start))
        while (and matched (< (1+ matched) end))
        count matched))

(defun qsas-make-abbrev-regexp (abbrev)
  (apply 'concat
         "^"
         (loop for char across (downcase abbrev) collect
               (format ".*?\\(%s\\)"
                       (regexp-quote (string char))))))

(defun qsas-abbrev-penalty (string skip-start skip-end)
  (let ((skipped (- skip-end skip-start)))
    (cond
     ((zerop skipped) 0)
     ((string-match "[ \\t\\r\\n_-]+$" (substring string skip-start skip-end))
      (let ((seps (- (match-end 0) (match-beginning 0))))
        (+ seps (* (- skipped seps) 0.15))))
     ((let ((case-fold-search nil))
        (eq (string-match "[[:upper:]]" string skip-end) skip-end))
      (let ((ups (let ((case-fold-search nil))
                   (count-matches-in-string
                    "[[:upper:]]" string skip-start skip-end))))
        (+ ups (* (- skipped ups) 0.15))))
     (t skipped))))

(defun qsas-abbrev-score-nocache (string abbrev)
  (cond
   ((zerop (length abbrev))             0.9)
   ((< (length string) (length abbrev)) 0.0)
   ((let ((case-fold-search t))
      (string-match (make-abbreviation-regexp abbrev) string))
    (loop with groups = (cddr (match-data))
          while groups
          for prev    = 0 then end
          for start   = (pop groups)
          for end     = (pop groups)
          for matched = (- end start)
          for skipped = (- start prev)
          for penalty = (qsas-abbrev-penalty string prev start)
          sum (+ matched (- skipped penalty)) into point
          finally return
          (let* ((length (length string))
                 (rest (- length end)))
            (/ (+ point (* rest 0.9)) (float length)))))
   (t 0.0)))

(defvar qsas-abbrev-score-cache
  (make-hash-table :test 'equal :weakness t))

(defun qsas-abbrev-score (string abbrev)
  (let ((cache-key (cons string abbrev)))
    (or (gethash cache-key qsas-abbrev-score-cache)
        (puthash cache-key
                 (qsas-abbrev-score-nocache string abbrev)
                 qsas-abbrev-score-cache))))

(provide 'qsas)
;;; qsas.el ends here
