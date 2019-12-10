;;; package --- Summary

;;; Commentary:
;; Putting org settings into its separate file.

;;; Code:
(require 'org)

;; remove tags from the agenda view
(setq org-agenda-remove-tags t)

;; START indented
(setq org-startup-indented t)

;; START folded
(setq org-startup-folded t)

;; START hidden asterisks
(setq org-hide-leading-stars t)

;; basic agenda stuff
(setq org-directory "~/Nextcloud/org")
(setq org-agenda-files "~/Nextcloud/org/agenda-files.org")
(setq org-default-notes-file (concat org-directory "/refile.org"))
(setq diary-file "~/Nextcloud/org/emacs-diary")
;; set day view as default
(setq org-agenda-span 'day)

(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done 'time)
(setq org-agenda-include-diary t)
(setq org-return-follows-link t) ;; not working! use C-c C-o instead - that is from org and it works


;; enable python for Org-babel (doing source code stuff in org mode)
     (org-babel-do-load-languages
      'org-babel-load-languages
      '((emacs-lisp . nil)
        (python . t)))

;; Enable state logging enable for the TODO state in org mode i.e. the "!" part
 (setq org-todo-keywords
       (quote
        ((sequence "TODO(t!)" "NEXT(n!)" "READ(r!)" "WRITE(o!)" "FIXME(f!)" "BUG(b!)" "|" "DONE(d!)")
         (sequence "WAITING(w@/!)" "SOMEDAY(s@/!)" "|" "CANCELLED(c@/!)"))))

;; Capture shit
(define-key global-map "\C-cc" 'org-capture)
(setq org-default-notes-file "~/Nextcloud/org/refile.org")
(setq org-capture-templates
      (quote (("z" "Todo" entry (file "~/Nextcloud/org/todo.org")
               "* TODO %?")
              ("o" "On-call log" entry (file "~/Nextcloud/org/oncall-logs.org")
               "* TODO %? :oncall:\nDetails of alert: \nEntered on %U\n")
              ("j" "Journal" entry (file+datetree "~/Nextcloud/org/journal.org")
               "* %?\nEntered on %U\n %i\n %a")
              ("e" "Emacs Tip" entry (file+headline "~/Nextcloud/org/emacs-tips.org" "Emacs Tips")
               "* %?\n %i\n %a")
              ("r" "RAM TODO/FIXME/BUG" entry (file+headline "~/Nextcloud/org/ram.org" "Tasks")
               "* %?\n%a"))))


;; test for colourising TODO tags
(setq org-tag-faces
      (quote (("code" :foreground "green yellow")
          ("read" :foreground "orange")
          ("urgent" :foreground "red" :background "white" :underline t)
          ("laptop" :foreground "orange")
          ("home" :foreground "orchid")
          ("work" :foreground "tomaton" :underline t)
          ("ram" :foreground "orange red")
          ("brainstorm" :foreground "aquamarine")
          ("research" :foreground "mint cream")
          ("oncall" :foreground "yellow" :weight bold)
          ("repeating" :foreground "pale turquoise")
          ("phone" :foreground "hot pink"))))

;; remap some keys
;; (global-set-key (kbd "<f1>") 'org-agenda)

;; colourize the headline tags
(setq org-todo-keyword-faces
      (quote (("TODO" :foreground "red" :weight bold)
              ("NEXT" :foreground "cyan" :weight bold)
          ("FIXME" :foreground "yellow" :weight bold)
          ("BUG" :foreground "yellow" :weight bold)
              ("DONE" :foreground "forest green" :weight bold)
              ("READ" :foreground "orange" :weight bold)
              ("WRITE" :foreground "magenta" :weight bold)
              ("CANCELLED" :foreground "forest green" :weight bold)
              ("WAITING" :foreground "MediumOrchid1" :weight bold)
              ("SOMEDAY" :foreground "forest green" :weight bold))))

;; use fast selection
(setq org-use-fast-todo-selection t)

;; getting much of this stuff form http://doc.norang.ca/org-mode.html by the way
;; use shift and arrows to change stage a TODO item without logging the change, which is great for fixing mistakes
(setq org-treat-S-cursor-todo-selection-as-state-change nil)

;; tag stuff automatically dependent on a change of state
(setq org-todo-state-tags-triggers
      (quote (("CANCELLED" ("CANCELLED" . t))
              ("WAITING" ("WAITING" . t))
              ("HOLD" ("WAITING") ("HOLD" . t))
              (done ("WAITING") ("HOLD"))
              ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
              ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
              ("DONE" ("WAITING") ("CANCELLED") ("HOLD")))))


;; START ---STUFF PULLED FROM emacs-leuven.el ----START
;; REMOVE THIS SECTION AND LOAD emacs-leuven.el again in init.el
;; if you want to go back to full emacs-leuven config

;; Settings for time grid for agenda display.
(setq org-agenda-time-grid '((daily remove-match)
                               ""
                               (0600 0800 1000 1200 1400 1600 1800 2000 2200)))

;; String for the current time marker in the agenda.
(setq org-agenda-current-time-string "Right now")

;; 10.4.3 Sorting structure for the agenda items of a single day.
(setq org-agenda-sorting-strategy   ; custom value
      '((agenda time-up category-up priority-down effort-down)
        (todo category-up priority-down effort-down)
        (tags category-up priority-down effort-down)
        (search category-up)))

;; Format string for displaying dates in the daily/weekly agenda
;; and in the timeline.
(setq org-agenda-format-date
      (concat                         ; "\n"
              "%Y-%m-%d" " %a "
              ;; (make-string (1- (window-width)) (string-to-char "_"))))
              (make-string 65 (string-to-char " "))
              "_"
              ;; (make-string 1 ?\u25AE)
              ))

  ;; Faces for specific Priorities (#A, #B and #C).
  (setq org-priority-faces
        '((?A . (:foreground "#CC0000" :background "#FFE3E3"))
          (?B . (:foreground "#64992C" :background "#EBF4DD"))
          (?C . (:foreground "#64992C" :background "#FFFFFF"))))

;; END ---STUFF PULLED FROM emacs-leuven.el ----END

(setq org-agenda-custom-commands
      (quote (("N" "Agenda and NEXT TODOs"
	       ((agenda "")
		(todo "NEXT")))
	      ("y" "Agenda and All TODOS"
	       ((agenda "")
		(alltodo ""))))

;; BELOW IS A BIGGER EXAMPLE OF THE FUNC ABOVE
;; setting custom views in the agenda ------- learned a lot about elist doing this!
;; (setq org-agenda-custom-commands
;;       (quote (("A" "Agenda and All TODOs"
;;                ((agenda "")
;;                 (alltodo "")))
;;               ("N" "Agenda and NEXT TODOs"
;;                ((agenda "")
;;                 (todo "NEXT")))
;;               ("G" "Agenda, NEXT and All TODOs"
;;                ((agenda "")
;;                 (todo "NEXT")
;;                 (alltodo "")))
;;               ("R" "Agenda and READ TODOs"
;;                ((agenda "")
;;                 (todo "READ")))
;;               ("c" "Code BUG/FIXME"
;;                ((agenda "")
;;                 (todo "BUG")
;;                 (todo "FIXME")))
;;               ("o" "On-call logs"
;;                ((agenda "")
;;                 (tags "oncall"
;;                       (quote (org-agenda-overriding-header "On Call Logs")))))
;;               ("r" "RAM stuff"
;;                ((agenda "")
;;                 (tags "ram"
;;                       (quote (org-agenda-overriding-header "RAM Stuff")))))
;;               ("H" "Agenda and Habits"
;;                ((agenda "")
;;                 (tags-todo "STYLE=\"habit\""
;;                        (quote (org-agenda-overriding-header "Habits"))))))))

;; use a single archive file for org
(setq org-archive-location "~/Nextcloud/org/archive/archive.org::* From %s")

;; refiling properly (or generally because everything is included)
(setq org-refile-targets (quote ((nil :maxlevel . 9)
                 (org-agenda-files :maxlevel . 9))))

(setq org-ellipsis "...")

;; END OF ORG MODE STUFF
