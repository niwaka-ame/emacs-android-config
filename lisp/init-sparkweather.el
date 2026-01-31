;;; init-sparkweather.el --- Custom configuration of sparkweather. -*- lexical-binding: t; -*-

(require 'sparkweather)

;; setq `calendar-latitude' and `calendar-longitude'
(load (concat emacs-dir "sparkweather-location.el"))
(setq sparkweather-add-footer nil)
(add-hook 'sparkweather-mode-hook (lambda () (text-scale-set -2)))

(defun sparkweather--calculate-column-width (entries)
  "Calculate width needed for first column based on ENTRIES.
Uses `string-width` to account for unicode display widths."
  (if entries
      (+ 2 (cl-loop for entry in entries
                    for columns = (cadr entry)
                    maximize (string-width (aref columns 0))))
    0))

(defun sparkweather--detect-invalid-windows (windows)
  "Detect windows with invalid hour ranges.
Patched to allow end-hour 24 (midnight) for the 23:00 slot."
  (cl-loop for window in windows
           for (name start end _face) = window
           for reason = (cond
                         ((or (< start 0) (> start 23) (< end 0) (> end 24))
                          'out-of-range)
                         ((<= end start)
                          'invalid-range))
           when reason
           collect (list name start end reason)))

(defun sparkweather-generate-hourly-windows ()
  "Generate hourly windows from the current hour until midnight."
  (let ((start-hour (decoded-time-hour (decode-time)))
        (windows nil))
    (cl-loop for current-hour from start-hour below 24
             for i from 0
             do (let* ((label (format "%02d:00" current-hour))
                       (next-hour (1+ current-hour))
                       (face (if (zerop i) 'warning 'success)))
                  (push (list label current-hour next-hour face) windows)))
    (nreverse windows)))

(defun sparkweather--display-window-entry (window-plist)
  "Create table entry with fixed spacing: Icon Time Temp Description."
  (let ((name (plist-get window-plist :name))
        (face (plist-get window-plist :face))
        (info (plist-get window-plist :weather-info))
        (temp (plist-get window-plist :temp))
        (unit (sparkweather--temperature-unit-symbol)))
    (when info
      (let ((desc (cadr info))
            (icon (propertize "■" 'face face))
            (temp-str (format "%d%s" (round temp) unit)))
        (list (intern (downcase name))
              (vector (concat icon " " name "     " temp-str "     " desc)
                      ""))))))

(defun sparkweather--create-entries (data current-hour windows)
  "Create table entries including full 24h sparklines and hourly breakdown."
  (pcase-let* ((`(,temps ,precip-probs ,temp-min ,temp-max ,precip-max ,rainy-codes)
                (sparkweather--calculate-ranges data))
               (converted-temps (mapcar #'sparkweather--convert-temperature temps))
               (`(,window-data ,highlights)
                (sparkweather--prepare-windows data windows))
               
               (temp-sparkline (sparkweather--sparkline converted-temps highlights current-hour))
               (precip-sparkline (sparkweather--sparkline precip-probs highlights current-hour))
               
               (worst-weather-code (and rainy-codes (apply #'max rainy-codes)))
               (worst-weather-info (when worst-weather-code
                                     (sparkweather--wmo-code-info worst-weather-code)))
               (worst-desc (if worst-weather-info (cadr worst-weather-info) ""))
               (unit (sparkweather--temperature-unit-symbol)))

    (append
     ;; Row 1: Today Summary
     (list (list 'range
                 (vector (string-trim
                          (format "Today: %d—%d%s %s"
                                  (round (sparkweather--convert-temperature temp-min))
                                  (round (sparkweather--convert-temperature temp-max))
                                  unit
                                  worst-desc))
                         "")))

     ;; Row 2: Temp Graph
     (list (list 'temp-graph (vector temp-sparkline "")))

     ;; Row 3: Precip Graph
     (list (list 'precip-graph (vector precip-sparkline "")))

     ;; Row 4+: Hourly Windows
     (cl-loop for window-plist in window-data
              for i from 0
              for hour-idx = (+ current-hour i)
              for hour-struct = (cl-find hour-idx data :key #'sparkweather-hour-hour)
              for raw-temp = (if hour-struct (sparkweather-hour-temperature hour-struct) 0)
              for conv-temp = (sparkweather--convert-temperature raw-temp)
              for entry-plist = (plist-put window-plist :temp conv-temp)
              for entry = (sparkweather--display-window-entry entry-plist)
              when entry collect entry))))

(defun sparkweather-day ()
  "Show full day weather with sparklines and hourly forecast."
  (interactive)
  (setq sparkweather-time-windows (sparkweather-generate-hourly-windows))
  (sparkweather--fetch-day #'sparkweather--display-day))

(provide 'init-sparkweather)
;;; init-sparkweather.el ends here
