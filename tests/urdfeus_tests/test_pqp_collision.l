;; Test template for pqp-collision-distance with robot object
;; This file is used by test_console_scripts.py
;;
;; Usage:
;;   EUS_FILE_PATH=/path/to/model.l ROBOT_FUNC_NAME=robot_name roseus test_pqp_collision.l
;;
;; Verifies that pqp-collision-distance returns the same results
;; whether called with robot object or individual links.

(defun run-collision-test (eus-file-path robot-func-name)
  (load eus-file-path)
  (setq robot (funcall (intern (string-upcase robot-func-name) *package*)))
  (setq link0 (car (send robot :links)))

  ;; Get bounding box from glvertices
  (setq bd (car (send link0 :bodies)))
  (setq glv (bd . gl::aglvertices))
  (setq mesh-list (glv . gl::mesh-list))

  ;; Calculate bounding box
  (let ((min-x 1e10) (max-x -1e10)
        (min-y 1e10) (max-y -1e10)
        (min-z 1e10) (max-z -1e10))
    (dolist (mesh mesh-list)
      (let ((verts-array (cadr (assoc :vertices mesh))))
        (when verts-array
          (dotimes (i (car (array-dimensions verts-array)))
            (let ((x (aref verts-array i 0))
                  (y (aref verts-array i 1))
                  (z (aref verts-array i 2)))
              (setq min-x (min min-x x) max-x (max max-x x))
              (setq min-y (min min-y y) max-y (max max-y y))
              (setq min-z (min min-z z) max-z (max max-z z)))))))

    ;; Test 1: Cube placed far outside mesh -> should NOT collide
    (setq cube-outside (make-cube 10 10 10))
    (send cube-outside :locate (float-vector (+ max-x 1000) 0 0) :world)
    (send cube-outside :make-pqpmodel :fat 0)
    (setq dist-outside-robot (car (pqp-collision-distance robot cube-outside)))
    (setq dist-outside-link (car (pqp-collision-distance link0 cube-outside)))

    ;; Test 2: Cube at mesh center -> test consistency between robot and link
    (setq center-x (/ (+ min-x max-x) 2.0))
    (setq center-y (/ (+ min-y max-y) 2.0))
    (setq center-z (/ (+ min-z max-z) 2.0))
    (setq cube-center (make-cube 10 10 10))
    (send cube-center :locate (float-vector center-x center-y center-z) :world)
    (send cube-center :make-pqpmodel :fat 0)
    (setq dist-center-robot (car (pqp-collision-distance robot cube-center)))
    (setq dist-center-link (car (pqp-collision-distance link0 cube-center)))

    ;; Test 3: Large cube covering the entire mesh
    (setq size-x (- max-x min-x))
    (setq size-y (- max-y min-y))
    (setq size-z (- max-z min-z))
    (setq cube-large (make-cube (* 2 size-x) (* 2 size-y) (* 2 size-z)))
    (send cube-large :locate (float-vector center-x center-y center-z) :world)
    (send cube-large :make-pqpmodel :fat 0)
    (setq dist-large-robot (car (pqp-collision-distance robot cube-large)))
    (setq dist-large-link (car (pqp-collision-distance link0 cube-large)))

    ;; Verify robot has pqpmodel set (our fix)
    (setq robot-has-pqpmodel (if (get robot :pqpmodel) 1 0))

    ;; Output results in CSV format for Python to parse
    (format t "RESULT:~A,~A,~A,~A,~A,~A,~A~%"
            dist-outside-robot
            dist-outside-link
            dist-center-robot
            dist-center-link
            dist-large-robot
            dist-large-link
            robot-has-pqpmodel)))

;; Main: get parameters from environment variables and run test
(let ((eus-file-path (unix:getenv "EUS_FILE_PATH"))
      (robot-func-name (unix:getenv "ROBOT_FUNC_NAME")))
  (when (and eus-file-path robot-func-name)
    (run-collision-test eus-file-path robot-func-name)
    (exit 0)))
