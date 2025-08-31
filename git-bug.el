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
                   (propertize (format "%.7s %.10s %s" id date (string-join (nthcdr 2 fields) "\t"))
                               'bug-id id
                               'bug-status status
                               'bug-date date
                               'bug-title title)))
                 bugs)))

(defun git-bug-extract-id-in-text ()
  "Find gb# on current line.  Search should match output of `git-bug-insert-bugid`."
  ;; TODO: be smarter. maybe search in both directions?
  ;; TODO: make 'gb#' prefix a package customize variable
  (save-excursion
           (move-beginning-of-line 1)
           (search-forward "gb#" (point-at-eol) t)
           (thing-at-point 'word t)))

(defun git-bug-completing-read ()
  "Completing-read for git-bug."
  (let ((init-input (git-bug-extract-id-in-text)))
    (when-let (selection (completing-read "bug:" (git-bug-candidates) nil nil init-input nil))
      ;; NB. splitting on space as saved from `git-bug-completing-read` vs tab from command output.
      (car (split-string selection " ")) ;; returns just the bug id.

      ;; completing-read does not return object with text properties?
      ;; (setq bugid (get-text-property 0 'bug-id selection))
      )))

(defun git-bug-insert-bugid (&optional bugid title)
  "Insert gb# issue number at current position in buffer.
Give `BUGID` and/or `TITLE` to avoid calling git-bug."
  (interactive "P")
  (when (not bugid) (setq bugid (git-bug-completing-read)))
  (insert (format "gb#%.7s" bugid))
  (when title (insert (format " \"%s\"" title)))
  ;; (insert " ")
  )

(defun git-bug-ls ()
  "Create a git-bug ls buffer."
  (interactive)
  (with-current-buffer (get-buffer-create "git-bug.org")
    (erase-buffer)
    (insert (shell-command-to-string "git-bug bug -f org-mode"))
    (goto-char (point-min))
    (pop-to-buffer (current-buffer))
    (org-mode)))

(defun git-bug-show-bug (&optional bugid)
  "Create a new buffer to show this `bugid`'s title and it's comments."
  (interactive "P")
  (when (not bugid) (setq bugid (git-bug-completing-read)))
  (with-current-buffer (get-buffer-create (format "gb#%s" bugid))
    (erase-buffer)
    (insert (shell-command-to-string
             (format "git-bug bug comment \"%s\"" bugid)))
    (goto-char (point-min))
    (pop-to-buffer (current-buffer))))

(defun git-bug-edit-bug (bugid)
  "Edit bug `BUGID` in temporary buffer.
Currently only works well if EDITOR is set to emacsclient and emacs-server is srunning.
TODO(gb#cc5fa60): refactor new and edit so edit can reuse temp buffer"
  (start-process (format "git-bug-edit:%s" bugid)
                 nil
                 "git-bug" "bug" "title" "edit" bugid))

(defun git-bug-edit-at-line ()
  "Edit first match of gb#1234567 on the current line."
  (interactive)
  (if-let
      ((bugid (git-bug-extract-id-in-text)))
      (git-bug-edit-bug bugid)
    (error "No bug like gh#1234567 on line")))

(defun git-bug-new-bug (&optional title message no-edit)
  "Create a new bug using a temporary buffer.  Insert optional `TITLE` and `MESSAGE` when given."
  (error "not implemented. See git-bug-editmsg-new")
  ;; TODO: create temp buffer and hooks
  (if no-edit (insert (format "%s\n\n%s" title message)))
  (shell-command-to-string (format "git-bug bug new -F \"%s\"" tempfile)))

(defun git-bug-cmd (bugid cmd)
  "Apply `CMD` to `BUGID`.
Runs e.g. `git-bug bug status open $bugid`."
  (shell-command-to-string (format "git-bug bug %s \"%s\"" cmd bugid)))

(defvar git-bug-menu-actions-alist
  '(;; ("test" . (lambda (bid) (message "selected: %s" bid)))
    ("show" . git-bug-show-bug)
    ("edit" . git-bug-edit-bug)
    ("insert" . git-bug-insert-bugid)
    ("close". (lambda (bugid) "Close issue." (git-bug-cmd "state close" bugid)))
    ("open" . (lambda (bugid) "Reopen issue." (git-bug-cmd "state open"  bugid))))
 "Bug actions preformed when given a `BUGID`.")

(defun git-bug-menu (&optional bugid action)
  "Choose a bug and than action each from a list.
Runs `ACTION` (in `git-bug-menu-actions-alist`) on `BUGID` (`git-bug bug -f json`).
`completing-read` for `BUGID` and/or `ACTION` if not provided."
  (interactive)
  (when (not bugid) (setq bugid (git-bug-completing-read)))
  (when (not bugid) (error "failed to select a bug id"))
  ;; TODO(gb#7b002ae): need to exit and return when bug is saved. also regexp is wrong?
  ;; (when (not (string-match "^[A-Za-z0-9]{9}$" bugid))
  ;;            (setq bugid (git-bug-editmsg-new bugid)))
  (when (not action)
    (setq action (cdr (assoc
                       (completing-read "Action:" git-bug-menu-actions-alist)
                       git-bug-menu-actions-alist))))

  (funcall action bugid))

;; Edit buffer a la magit, org-capture, etc
(define-minor-mode git-bug-editmsg-mode
  "A minor mode for committing temporary edits."
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c C-c") #'git-bug-editmsg-save-and-close)
            (define-key map (kbd "C-c C-k") #'git-bug-editmsg-close)
            map)
  :lighter "GBug")

(defun git-bug-editmsg-close (&optional no-kill)
  "Close edit buffer and remove the underlining file without save questions.
`NO-KILL` is set to t when called from `kill-buffer-hook` to avoid recursion."
  (interactive)
  ;; Dont do dangerous things to the wrong buffer
  (if (not (bound-and-true-p git-bug-editmsg-mode))
      (error "`git-bug-editmsg-close` when not in `git-bug-editmsg-mode`! ignorring"))
  (let ((temp-file  (buffer-file-name)))
    ;; kill-buffer hook or keymap trigger? avoid accidental recursion
    (when (not no-kill)
      (set-buffer-modified-p nil) ;; don't prompt to save
      (kill-buffer (current-buffer)))

    ;; did we already delete the file? yes if C-c C-k?
    (when (and temp-file (file-exists-p temp-file))
      (delete-file temp-file))))

(defun git-bug-editmsg-new (&optional initial-text)
  "Open a new temporary \"BUG_MESSAGE\" file with `INITIAL-TEXT`.
Hook removes file when buffer is killed."
  (interactive)
  (let ((cur-dir default-directory)
        (temp-file (make-temp-file "BUG_MESSAGE_EDITMSG" nil nil initial-text)))
    (find-file temp-file)
    ;; pretend to be where we came from so git commands work
    (setq-local default-directory cur-dir)

    (goto-char (point-at-eol))
    (insert "\n\n# C-C C-c to save bug; C-C C-k to discard.")
    (goto-char (point-min))
    (git-bug-editmsg-mode 1)
    (add-hook 'kill-buffer-hook (lambda () (git-bug-editmsg-close t)) nil t)))

(defun git-bug-editmsg-save-and-close ()
  "Use current buffer as input to git bug new and kill it.
Hook should also remove what is assumed to be a temporary file.
Returns git-bug id."
  (interactive)
  (save-buffer)
  (let* ((cmd (format "git-bug bug new -F \"%s\"" (buffer-file-name)))
         (cmd-res (shell-command-to-string cmd))
         (bugid (replace-regexp-in-string " created\n" "" cmd-res)))
    (when (or (not bugid) (string= bugid ""))
      (error "Failed to create a bug?! No id returned by '%s' => '%s'" cmd cmd-res))
    (message "new git-bug bugid: %s" bugid)
    (git-bug-editmsg-close)
    bugid))

(defun git-bug-new-from-line ()
  "Create a bug from TODO/FIX/BUG: on the current line.
Abuse `git-bug-editmsg-new` and `git-bug-editmsg-save-and-close` as hidden buffers to run git-bug bug new command."
  (interactive)
  (save-excursion
    (move-beginning-of-line 1)
    (search-forward-regexp "TODO:\\|FIX:\\|BUG:" (point-at-eol) t)
    (let ((label-point (point)))
      (if (= label-point (point-at-bol))
          (error "No label like TODO FIX or BUG found."))
      (let* ((bug-title (string-trim (buffer-substring-no-properties label-point (point-at-eol))))
             (bugid
              (save-current-buffer
                (git-bug-editmsg-new bug-title)
                (git-bug-editmsg-save-and-close))))
        (goto-char (- label-point 1)) ;; go back before the trailing ':'

        ;; final output looks like 'TODO(gb#1234567): title of bug/issue'
        (insert "(")
        (git-bug-insert-bugid bugid)
        (insert ")")
        ))))


;;; TODOs
;; TODO(: quick line to bug from TODO FIX BUG
;; TODO(gb#e7a8b7c): edit message color like commit-message
;; TODO(gb#94e034c): git-bug porcelain for magit-forge
;; TODO(gb#6588bc5): list of git-bug project directories for overview of all page
;; TODO(gb#3a93c2e): minor-mode for clickable buttons, company/cornfu completion? (adjust

(provide git-bug)
;; git-bug.el ends here
