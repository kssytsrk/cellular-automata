(in-package #:ca)

(clim:define-application-frame ca-ui-app () ()
  (:panes
   (my-interactor :interactor
                  :height 400
                  :height 600))
  (:layouts
   (my-default my-interactor)))

(defun run-ca-ui-app ()
  (clim:run-frame-top-level (clim:make-application-frame 'ca-ui-app)))
