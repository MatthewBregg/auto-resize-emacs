;;; auto-resize.el --- Auto-resize emacs!
;;; Version: 1.0
;;; package  --- Summary : Auto resizes emacs based on the ppi of the current window it is in.
;;; Commentary: 
;; Autoresizes based on current ppi, using the frame attributes.
;; This now uses a timer, to consistently check every .3 seconds by default, which is customizable. 
;; ALSO ONLY TESTED ON LINUX ATM 
;; Activates automatically if installed, might add a hook to turn on/off later

;;; Code:


(defgroup auto-resize nil
  "A package to auto-resize emacs when moving between high/low dpi monitors"
  :group 'convenience
  :link '(url-link "https://github.com/MatthewBregg/auto-resize-emacs")
  :version '1.1
  )

;;How to set font : Sets font for the current fram, and only via height
(defun auto-resize--set-font-height (size)
  ;;Set main font
  (set-face-attribute 'default 
                      (selected-frame)
                      :height size)
  ;;Set modeline font
  (set-face-attribute 'mode-line 
                      (selected-frame)
                      :height size)
  )



;;Add dpi calculator also, so size, rather than raw res

(defun auto-resize--get-current-monitor-resolution ()
  (let ((monitor (frame-monitor-attributes)))
    (if (eq monitor nil) nil
   (cdr (cdr (cdr (cadr monitor)))))))


(defcustom auto-resize--ppi-turning-point 14200
  "The ppi for which when above, switch scaling on" :group 'auto-resize)

(defun auto-resize--current-monitor-high-dpi? ()
  (let ((resolution (auto-resize--get-current-monitor-resolution)))
      ;;Check here if the monitor is high dpi, improve this function alter
      (if (> (auto-resize--get-current-monitor-ppi) auto-resize--ppi-turning-point) t nil)))

(defun auto-resize--get-current-monitor-dimensions ()
  (cdr (cadr (cdr (cdr (frame-monitor-attributes))))))

(defun auto-resize--get-current-monitor-area-in-inches ()
  (let ((dimensions (auto-resize--get-current-monitor-dimensions)))
    (if (or (not (numberp (car dimensions))) (not (numberp (cadr dimensions) ))) nil
    (* (auto-resize--mmtoinches (car dimensions)) (auto-resize--mmtoinches (cadr dimensions))))))



(defun auto-resize--get-current-pixel-count ()
  (let ((resolution (auto-resize--get-current-monitor-resolution)))
    (if (or (eq resolution nil) (not (numberp (car resolution))) (not (numberp (cadr resolution))))
        nil
      (* (car resolution) (cadr resolution))
    )))

(defun auto-resize--get-current-monitor-ppi ()
  (let ((pixel-count (auto-resize--get-current-pixel-count))
        (area-in-inches (auto-resize--get-current-monitor-area-in-inches))
        )
    (if (or (numberp (numberp pixel-count)) (not (numberp area-in-inches))) nil
       (/  pixel-count  area-in-inches))))

(defvar auto-resize--last-saw-ppi 1000
  "The last ppi that was checked against")

(defcustom auto-resize--normal-size 100
  "The size to go to for low-dpi displays" :group 'auto-resize)

(defcustom auto-resize--high-size 200
  "The size to go to for high-dpi displays" :group 'auto-resize)

(defun auto-resize--resize-frame () 
  (setq auto-resize--last-saw-ppi (auto-resize--get-current-monitor-ppi))
  (if (auto-resize--current-monitor-high-dpi?)
      (auto-resize--set-font-height auto-resize--high-size)
    (auto-resize--set-font-height auto-resize--normal-size))
  'resized)


(defun auto-resize--resize-current-frame-if-new ()
  (let ((current-monitor-ppi (auto-resize--get-current-monitor-ppi)))
  (if (not (numberp current-monitor-ppi)) nil
  (when (display-graphic-p)
    (if (= auto-resize--last-saw-ppi current-monitor-ppi)  'didnt-resize (auto-resize--resize-frame)))
  )))

(defun auto-resize--mmtoinches (x)
  (/ x 25.4))

;(add-hook 'window-configuration-change-hook (lambda () (auto-resize--resize-current-frame-if-new)))
(defcustom auto-resize--resize-check-frequency .3
  "How often to check for resize, in seconds.
Uses 'sleep-for' to wait, subject to same restrictions as that function.")
(defvar auto-resize--timer-value nil "Timer value, for canceling.")
(defun auto-resize-cancel-auto-resize-timer ()
  "Stop the auto-resize-check."
  (interactive)
  (if auto-resize--timer-value (progn
                                 (cancel-timer auto-resize--timer-value)
                                 (setq auto-resize--timer-value nil))))
(defun auto-resize-start-auto-resize-timer ()
  "Start the auto-resize check if it isn't running."
  (interactive)
  (if auto-resize--timer-value nil (setq auto-resize--timer-value (run-at-time "1 sec" auto-resize--resize-check-frequency 'auto-resize--resize-current-frame-if-new)))
  )
(auto-resize-start-auto-resize-timer)	
(provide 'auto-resize)
;;; auto-resize.el ends here
