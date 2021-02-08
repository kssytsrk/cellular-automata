(in-package #:ca)

(defstruct (world (:constructor %make-world))
  "A world structure."
  (name "default")
  (rows 20)
  (cols 20)
  (states '(:off :on))
  (array nil))

(defun make-world (&key
                   (name "default")
                   (rows 20) (cols 20)
                   (states '(:off :on))
                   (initial-element (first states)))
  (%make-world :name name
               :rows rows
               :cols cols
               :states states
               :array (make-array rows
                                  :initial-contents
                                  (loop for i from 0 below rows
                                        collect (make-array cols
                                                            :initial-element
                                                            initial-element)))))

(defmethod world-aref ((world world) row col)
  (aref (aref (world-array world) row) col))

(defun set-world-aref (world row col wanted-state)
  (setf (aref (aref (world-array world) row) col) wanted-state))

(defsetf world-aref set-world-aref)

(defun calculate-world (world ruleset &optional (start 0))
  (loop for i from start to (- (length (world-array world)) 2)
        do (setf (aref (world-array world) (1+ i))
                 (calculate-next-generation (aref (world-array world) i) ruleset)))
  world)

(defun calculate-next-generation (current-generation ruleset)
  (let ((next-generation (make-array (length current-generation))))
    (dotimes (i (length current-generation))
      (setf (aref next-generation i)
            (rest (assoc (list (handler-case
                                      (aref current-generation (1- i))
                                    (t () 0))
                                  (aref current-generation i)
                                  (handler-case
                                      (aref current-generation (1+ i))
                                    (t () 0)))
                            ruleset :test #'equal))))
    next-generation))
