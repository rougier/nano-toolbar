
(defun mu4e-quick-select ()
  (interactive)
  (let ((selection
         (nano-toolbar " SELECT" '(("UNREAD"    . active)
                                   ("TODO"      . default)
                                   ("INBOX"     . default)
                                   ("TODAY"     . default)
                                   ("FLAGGED"   . default)
                                   ("YESTERDAY" . default)
                                   ("LAST WEEK" . default)))))
    
    (cond ((string= selection "UNREAD")
           (mu4e-search "flag:unread"))
          
          ((string= selection "INBOX")
           (mu4e-search "m:/inria/inbox or m:/gmail/inbox or m:/univ/inbox"))
          
          ((string= selection "TODO")
           (mu4e-search "tag:TODO"))
          
          ((string= selection "TODAY")
           (mu4e-search "date:today..now"))
          
          ((string= selection "FLAGGED")
           (mu4e-search "flag:flagged"))
          
          ((string= selection "YESTERDAY")
           (mu4e-search "date:2d..today and not date:today..now"))
          
          ((string= selection "LAST WEEK")
           (mu4e-search "date:2d..today and not date:today..now")))))
          
(bind-key "H-s" #'mu4e-quick-select 'mu4e-headers-mode-map)
