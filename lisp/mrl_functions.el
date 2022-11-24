;; for markdown mode
(defun mrl/get-start-and-lines-in-region ()
  "Cumbersome newbie way to get start line and number
of lines in region."
  (let (pos1 pos2)
    (setq pos1 (region-beginning)
          pos2 (region-end))
    (let ((line1 (line-number-at-pos pos1))
          (line2 (line-number-at-pos pos2)))
      (list line1 (- line2 line1)))))

(defun mrl/clear-check-region (start lines)
  "Clears check marks given a start line and number
of lines to run through."
  (setq count 0)
  (while (> lines 0)
    (save-excursion
      (goto-line (+ start count))
      (beginning-of-line)
      (forward-char 3)
      (delete-char 1)
      (insert-char ?\s))
    (next-line)
    (setq count (+ count 1))
    (setq lines (- lines 1))))


(defun mrl/clear-check-from-region ()
  "Interactive func to clear markdown checks from a region."
  (interactive)
  (save-excursion
    (mrl/clear-check-region
     (car (mrl/get-start-and-lines-in-region))
     (car (last (mrl/get-start-and-lines-in-region))))))

;; do this for a region next
(defun mrl/clear-check-single-line (&optional lines)
  "Remove the check from LINES number of markdown checkbox lines."
  (interactive "p")
  (while (> lines 0)
    (save-excursion
      (beginning-of-line)
      (forward-char 3)
      (delete-char 1)
      (insert-char ?\s))
    (next-line)
    (setq lines (- lines 1))))
(provide 'mrl_functions)
;;; mrl_functions ends here
