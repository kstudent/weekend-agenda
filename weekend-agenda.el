(require 'org)
(require 'org-agenda)
(require 'cl-lib)


(defun org-agenda-list (&optional arg start-day span with-hour)
  "Produce a daily/weekly view from all files in variable `org-agenda-files'.
The view will be for the current day or week, but from the overview buffer
you will be able to go to other days/weeks.

With a numeric prefix argument in an interactive call, the agenda will
span ARG days.  Lisp programs should instead specify SPAN to change
the number of days.  SPAN defaults to `org-agenda-span'.

START-DAY defaults to TODAY, or to the most recent match for the weekday
given in `org-agenda-start-on-weekday'.

When WITH-HOUR is non-nil, only include scheduled and deadline
items if they have an hour specification like [h]h:mm."
  (interactive "P")
  (if org-agenda-overriding-arguments
      (setq arg (car org-agenda-overriding-arguments)
	    start-day (nth 1 org-agenda-overriding-arguments)
	    span (nth 2 org-agenda-overriding-arguments)))
  (if (and (integerp arg) (> arg 0))
      (setq span arg arg nil))
  (catch 'exit
    (setq org-agenda-buffer-name
	  (or org-agenda-buffer-tmp-name
	      (and org-agenda-doing-sticky-redo org-agenda-buffer-name)
	      (if org-agenda-sticky
		  (cond ((and org-keys (stringp org-match))
			 (format "*Org Agenda(%s:%s)*" org-keys org-match))
			(org-keys
			 (format "*Org Agenda(%s)*" org-keys))
			(t "*Org Agenda(a)*")))
	      "*Org Agenda*"))
    (org-agenda-prepare "Day/Week")
    (setq start-day (or start-day org-agenda-start-day))
    (if (stringp start-day)
	;; Convert to an absolute day number
	(setq start-day (time-to-days (org-read-date nil t start-day))))
    (org-compile-prefix-format 'agenda)
    (org-set-sorting-strategy 'agenda)
    (let* ((span (org-agenda-ndays-to-span (or span org-agenda-span)))
	   (today (org-today))
	   (sd (or start-day today))
	   (ndays (org-agenda-span-to-ndays span sd))
	   (org-agenda-start-on-weekday
	    (if (or (eq ndays 7) (eq ndays 14))
		org-agenda-start-on-weekday))
	   (thefiles (org-agenda-files nil 'ifmode))
	   (files thefiles)
	   (start (if (or (null org-agenda-start-on-weekday)
			  (< ndays 7))
		      sd
		    (let* ((nt (calendar-day-of-week
				(calendar-gregorian-from-absolute sd)))
			   (n1 org-agenda-start-on-weekday)
			   (d (- nt n1)))
		      (- sd (+ (if (< d 0) 7 0) d)))))
	   (day-numbers (list start))
	   (day-cnt 0)
	   (inhibit-redisplay (not debug-on-error))
	   (org-agenda-show-log-scoped org-agenda-show-log)
	   s e rtn rtnall file date d start-pos end-pos todayp
	   clocktable-start clocktable-end filter)
      (setq org-agenda-redo-command
	    (list 'org-agenda-list (list 'quote arg) start-day (list 'quote span) with-hour))
      (dotimes (n (1- ndays))
	(push (1+ (car day-numbers)) day-numbers))
      (setq day-numbers (nreverse day-numbers))
      ;;not needed at this point.
      ;;      (setq day-numbers (kree/remove-workdays day-numbers))
      (setq clocktable-start (car day-numbers)
	    clocktable-end (1+ (or (org-last day-numbers) 0)))
      (setq-local org-starting-day (car day-numbers))
      (setq-local org-arg-loc arg)
      (setq-local org-agenda-current-span (org-agenda-ndays-to-span span))
      (unless org-agenda-compact-blocks
	(let* ((d1 (car day-numbers))
	       (d2 (org-last day-numbers))
	       (w1 (org-days-to-iso-week d1))
	       (w2 (org-days-to-iso-week d2)))
	  (setq s (point))
	  (if org-agenda-overriding-header
	      (insert (org-add-props (copy-sequence org-agenda-overriding-header)
			  nil 'face 'org-agenda-structure) "\n")
	    (insert (org-agenda-span-name span)
		    "-agenda"
		    (if (< (- d2 d1) 350)
			(if (= w1 w2)
			    (format " (W%02d)" w1)
			  (format " (W%02d-W%02d)" w1 w2))
		      "")
		    ":\n")))
	(add-text-properties s (1- (point)) (list 'face 'org-agenda-structure
						  'org-date-line t))
	(org-agenda-mark-header-line s))
      (while (setq d (pop day-numbers))
	(setq date (calendar-gregorian-from-absolute d)
	      s (point))
	(if (or (setq todayp (= d today))
		(and (not start-pos) (= d sd)))
	    (setq start-pos (point))
	  (if (and start-pos (not end-pos))
	      (setq end-pos (point))))
	(setq files thefiles
	      rtnall nil)
	(while (setq file (pop files))
	  (catch 'nextfile
	    (org-check-agenda-file file)
	    (let ((org-agenda-entry-types org-agenda-entry-types))
	      ;; Starred types override non-starred equivalents
	      (when (member :deadline* org-agenda-entry-types)
		(setq org-agenda-entry-types
		      (delq :deadline org-agenda-entry-types)))
	      (when (member :scheduled* org-agenda-entry-types)
		(setq org-agenda-entry-types
		      (delq :scheduled org-agenda-entry-types)))
	      ;; Honor with-hour
	      (when with-hour
		(when (member :deadline org-agenda-entry-types)
		  (setq org-agenda-entry-types
			(delq :deadline org-agenda-entry-types))
		  (push :deadline* org-agenda-entry-types))
		(when (member :scheduled org-agenda-entry-types)
		  (setq org-agenda-entry-types
			(delq :scheduled org-agenda-entry-types))
		  (push :scheduled* org-agenda-entry-types)))
	      (unless org-agenda-include-deadlines
		(setq org-agenda-entry-types
		      (delq :deadline* (delq :deadline org-agenda-entry-types))))
	      (cond
	       ((memq org-agenda-show-log-scoped '(only clockcheck))
		(setq rtn (org-agenda-get-day-entries
			   file date :closed)))
	       (org-agenda-show-log-scoped
		(setq rtn (apply 'org-agenda-get-day-entries
				 file date
				 (append '(:closed) org-agenda-entry-types))))
	       (t
		(setq rtn (apply 'org-agenda-get-day-entries
				 file date
				 org-agenda-entry-types)))))
	    (setq rtnall (append rtnall rtn)))) ;; all entries
	(if org-agenda-include-diary
	    (let ((org-agenda-search-headline-for-time t))
	      (require 'diary-lib)
	      (setq rtn (org-get-entries-from-diary date))
	      (setq rtnall (append rtnall rtn))))
	(setq old-keep-day (if (boundp 'keep-day) keep-day))
	(setq keep-day (kree/holiday-or-weekend-p rtnall d))
	(kree/add-separator keep-day old-keep-day)

	(if (and org-agenda-only-show-weekend-or-holiday (not day-numbers) keep-day)
	    (add-to-list 'day-numbers (+ d 1)))
	
	(if (and (or rtnall org-agenda-show-all-dates) keep-day)
	    (progn
	      (setq day-cnt (1+ day-cnt))
	      ;; print header
	      (insert
	       (if (stringp org-agenda-format-date)
		   (format-time-string org-agenda-format-date
				       (org-time-from-absolute date))
		 (funcall org-agenda-format-date date))
	       "\n")
	      (put-text-property s (1- (point)) 'face
				 (org-agenda-get-day-face date (kree/holiday-p rtnall)))
	      (put-text-property s (1- (point)) 'org-date-line t)
	      (put-text-property s (1- (point)) 'org-agenda-date-header t)
	      (put-text-property s (1- (point)) 'org-day-cnt day-cnt)
	      (when todayp
		(put-text-property s (1- (point)) 'org-today t))
	      (setq rtnall
		    (org-agenda-add-time-grid-maybe rtnall ndays todayp))
	      ;; print items 
	      (if rtnall (insert 
			  (org-agenda-finalize-entries rtnall 'agenda)
			  "\n"))
	      (put-text-property s (1- (point)) 'day d)
	      (put-text-property s (1- (point)) 'org-day-cnt day-cnt)))
	(setq old-d d))
      (when (and org-agenda-clockreport-mode clocktable-start)
	(let ((org-agenda-files (org-agenda-files nil 'ifmode))
	      ;; the above line is to ensure the restricted range!
	      (p (copy-sequence org-agenda-clockreport-parameter-plist))
	      tbl)
	  (setq p (org-plist-delete p :block))
	  (setq p (plist-put p :tstart clocktable-start))
	  (setq p (plist-put p :tend clocktable-end))
	  (setq p (plist-put p :scope 'agenda))
	  (setq tbl (apply 'org-clock-get-clocktable p))
	  (insert tbl)))
      (goto-char (point-min))
      (or org-agenda-multi (org-agenda-fit-window-to-buffer))
      (unless (and (pos-visible-in-window-p (point-min))
		   (pos-visible-in-window-p (point-max)))
	(goto-char (1- (point-max)))
	(recenter -1)
	(if (not (pos-visible-in-window-p (or start-pos 1)))
	    (progn
	      (goto-char (or start-pos 1))
	      (recenter 1))))
      (goto-char (or start-pos 1))
      (add-text-properties (point-min) (point-max)
			   `(org-agenda-type agenda
					     org-last-args (,arg ,start-day ,span)
					     org-redo-cmd ,org-agenda-redo-command
					     org-series-cmd ,org-cmd))
      (if (eq org-agenda-show-log-scoped 'clockcheck)
	  (org-agenda-show-clocking-issues))
      (org-agenda-finalize)
      (setq buffer-read-only t)
      (message ""))))

(defun org-agenda-view-mode-dispatch ()
  "Call one of the view mode commands."
  (interactive)
  (message "View: [d]ay  [w]eek  for[t]night  [m]onth  [y]ear  [SPC]reset  [q]uit/abort
      time[G]rid   [[]inactive  [f]ollow      [l]og    [L]og-all   [c]lockcheck
      [a]rch-trees [A]rch-files clock[R]eport include[D]iary       [E]ntryText
      holiday-or-[W]eekend-mode")
  (pcase (read-char-exclusive)
    (?\ (call-interactively 'org-agenda-reset-view))
    (?d (call-interactively 'org-agenda-day-view))
    (?w (call-interactively 'org-agenda-week-view))
    (?W (call-interactively 'kree/org-agenda-toggle-weekend-or-holiday-mode))
    (?t (call-interactively 'org-agenda-fortnight-view))
    (?m (call-interactively 'org-agenda-month-view))
    (?y (call-interactively 'org-agenda-year-view))
    (?l (call-interactively 'org-agenda-log-mode))
    (?L (org-agenda-log-mode '(4)))
    (?c (org-agenda-log-mode 'clockcheck))
    ((or ?F ?f) (call-interactively 'org-agenda-follow-mode))
    (?a (call-interactively 'org-agenda-archives-mode))
    (?A (org-agenda-archives-mode 'files))
    ((or ?R ?r) (call-interactively 'org-agenda-clockreport-mode))
    ((or ?E ?e) (call-interactively 'org-agenda-entry-text-mode))
    (?G (call-interactively 'org-agenda-toggle-time-grid))
    (?D (call-interactively 'org-agenda-toggle-diary))
    (?\! (call-interactively 'org-agenda-toggle-deadlines))
    (?\[ (let ((org-agenda-include-inactive-timestamps t))
	   (org-agenda-check-type t 'timeline 'agenda)
	   (org-agenda-redo))
	 (message "Display now includes inactive timestamps as well"))
    (?q (message "Abort"))
    (key (user-error "Invalid key: %s" key))))

(defun kree/org-agenda-toggle-weekend-or-holiday-mode ()
  "Toggle weekend-or-holiday-mode."
  (interactive)
  (org-agenda-check-type t 'agenda)
  (setq org-agenda-only-show-weekend-or-holiday (not org-agenda-only-show-weekend-or-holiday))
  (org-agenda-redo)
  (org-agenda-set-mode-name)
  (message "Weekend-or-holiday mode turned %s"
	   (if org-agenda-only-show-weekend-or-holiday "on" "off")))

(setq org-agenda-only-show-weekend-or-holiday nil)

(defun kree/holiday-or-weekend-p (rtnall d)
  (or (not org-agenda-only-show-weekend-or-holiday) (kree/weekend-p d) (kree/holiday-p rtnall)))

(defun kree/weekend-p (d)
  (memq (calendar-day-of-week (calendar-gregorian-from-absolute d)) org-agenda-weekend-days))

(defun kree/holiday-p (rtnall)
  (if (cl-find-if 'kree/item-has-tag-p rtnall) t)
  )

(defun kree/item-has-tag-p (item)
  (string-match-p (regexp-quote ":holiday:") item))

(defun kree/add-separator(keep old-keep)
  (message "keep: %s old-keep: %s" keep old-keep)
  (if (and org-agenda-only-show-weekend-or-holiday (not old-keep) keep)
      (insert "------------------------------------------\n")))

(defun org-agenda-get-day-face (date is-holiday)
  "Return the face DATE should be displayed with."
  (cond ((and (functionp org-agenda-day-face-function)
	      (funcall org-agenda-day-face-function date)))
	((org-agenda-today-p date) 'org-agenda-date-today)
	((or (memq (calendar-day-of-week date) org-agenda-weekend-days) is-holiday)
	 'org-agenda-date-weekend)
	(t 'org-agenda-date)))
