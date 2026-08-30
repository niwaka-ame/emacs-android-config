;;; toolbar-spacer.el --- Description -*- lexical-binding: t; -*-

(defconst toolbar-spacer-xpm
  "/* XPM */
static char *spacer[] = {
\"1 1 1 1\",
\"  c None\",
\" \"
};")

(defun toolbar-spacer-item (width)
  `(menu-item "" ignore
              :enable nil
              :image (image
                      :type xpm
                      :data ,toolbar-spacer-xpm
                      :width ,width
                      :height 1)))


(provide 'toolbar-spacer)
;;; init-toolbar.el ends here
