(setq calendar-week-start-day 1)	; Weeks start on monday

(setq calendar-legal-holidays
      '((holiday-fixed 1 1 "New Year's Day")
        (holiday-fixed 12 25 "Christmas")))

(setq calendar-holidays
      `(,@holiday-solar-holidays
        ,@calendar-legal-holidays))
