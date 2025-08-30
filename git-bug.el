;;; package --- git-bug
;;; Commentary:
;; This file packages shell wrappers and convenience functions for git-bug.
;;; Code:
(defun git-bug-candidates ()
  "Shell out to git-bug.  Formatted id candidates for `ivy-read`."
  (let* ((cmd-bug-list-jq-tsv "git-bug bug -f json | jq -r '.[]|[.id,.edit_time.time,.author.name,.status,.title]|@tsv'|sort -k2,2nr")
         (res (shell-command-to-string cmd-bug-list-jq-tsv))
         (bugs (split-string res "\n" t))
         )
      (seq-map (lambda (line)
                 (let* ((fields (split-string line "\t"))
                        (id (nth 0 fields))
                        (date (nth 1 fields))
                        (status (nth 3 fields))
                        (title (nth 4 fields))
                        )
                   (propertize (format "%.5s %.10s %s" id date (string-join (nthcdr 2 fields) "\t"))
                               'bug-id id
                               'bug-status status
                               'bug-date date
                               'bug-title title)))
                 bugs)))

(defun git-bug-id-in-text ()
  "Find gb# on current line.  Search should match output of `git-bug-text-insert`."
  ;; TODO: be smarter. maybe search in both directions?
  ;; TODO: make 'gb#' prefix a package customize variable
  (save-excursion
           (move-beginning-of-line 1)
           (search-forward "gb#" (point-at-eol) t)
           (thing-at-point 'word t)))

(defun git-bug-completing-read ()
  "Completing-read for git-bug."
  (let ((init-input (git-bug-id-in-text)))
    (when-let (selection (completing-read "bug:" (git-bug-candidates) nil nil init-input nil))
      ;; NB. splitting on space as saved from `git-bug-completing-read` vs tab from command output.
      (car (split-string selection " ")) ;; returns just the bug id.

      ;; completing-read does not return object with text properties?
      ;; (setq bugid (get-text-property 0 'bug-id selection))
      )))

(defun git-bug-text-insert (&optional bugid title)
  "Insert gb# issue number give `BUGID` and/or `TITLE` to avoid calling git-bug."
  (interactive "P")
  (when (not bugid) (setq bugid (git-bug-completing-read)))
  (insert (format "gb#%.5s" bugid))
  (when title (insert (format " \"%s\"" title)))
  (insert " "))

(defun git-bug-ls ()
  "Create a git-bug ls buffer."
  (interactive)
  (with-current-buffer (get-buffer-create "git-bug.org")
    (erase-buffer)
    (insert (shell-command-to-string "git-bug bug -f org-mode"))
    (goto-char (point-min))
    (pop-to-buffer (current-buffer))
    (org-mode)))

(defun git-bug-view-bug (&optional bugid)
  "Create a new buffer with this `bugid` and it's comments."
  (interactive "P")
  (when (not bugid) (setq bugid (git-bug-completing-read)))
  (with-current-buffer (get-buffer-create (format "gb#%s" bugid))
    (erase-buffer)
    (insert (shell-command-to-string
             (format "git-bug bug comment \"%s\"" bugid)))
    (goto-char (point-min))
    (pop-to-buffer (current-buffer))))

(defun git-bug-edit-bug (&optional bugid)
  "Edit bug `BUGID` in temporary buffer."
  (error "not implemented"))

(defun git-bug-new-bug (&optional title message no-edit)
  "Create a new bug using a temporary buffer.  Insert optional `TITLE` and `MESSAGE` when given."
  (error "not implemented")
  ;; TODO: create temp buffer and hooks
  (if no-edit (insert (format "%s\n\n%s" title message)))
  (shell-command-to-string (format "git-bug bug new -F \"%s\"" tempfile)))

(defun git-bug-run-on-sel (func selection &rest rest)
  "Get bug-id from property in `SELECTION` and run `FUNC`.
`REST` is passed along to `FUNC` as additional arguments.

See `git-bug-candidates` for text properties.  Useful for ivy menu `git-bug-ivy-select`."
  (funcall func (get-text-property 0 'bug-id selection) ,@rest))

(defun git-bug-chstate (bugid state)
  "Set `STATE` (open or close) on `BUGID`.
Runs e.g. `git-bug bug status open $bugid`."
  (shell-command-to-string (format "git-bug bug status %s \"%s\"" state bugid)))

(defun git-bug-ivy-select ()
  "Use `ivy-read` to select a git-bug entry."
  (interactive)
  (let ((candidates (git-bug-candidates)))
    (ivy-read "Select git-bug: " candidates
              :action
              '(("v" (lambda (s) (git-bug-run-on-sel #'git-bug-view-bug s)))
                ("e" (lambda (s) (git-bug-run-on-sel #'git-bug-edit-bug s)))
                ("c" (lambda (s) (git-bug-run-on-sel #'git-bug-chstate s "close")))
                ("o" (lambda (s) (git-bug-run-on-sel #'git-bug-chstate s "open")))
                ))))


;;; TODOs
;; TODO: quick line to bug from TODO FIX BUG
;; TODO: git-bug porcelain for magit-forge
;; TODO: list of git-bug project directories for overview of all page
;; TODO: minor-mode for clickable buttons, company/cornfu completion? (adjust

(provide git-bug)
;; git-bug.el ends here
