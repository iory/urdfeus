;; eus2urdf_dump.l
;;
;; Loads an EusLisp robot model, instantiates it, zeroes all movable joints,
;; walks the whole link tree (including links connected by fixed joints and
;; links added procedurally in :init), and dumps the kinematics + mesh data
;; (glvertices) as JSON to a file.
;;
;; The following globals must be setq'ed before this file is loaded:
;;   *eus2urdf-model-path*   : path to the .l model file to (load)
;;   *eus2urdf-constructor*  : symbol/string naming the constructor function
;;   *eus2urdf-out-path*     : path to write the JSON dump to
;;
;; Leaving *eus2urdf-model-path* unset makes loading this file side-effect
;; free, which is how a live irteusgl session picks up (dump-object-json obj
;; path): the same dump, but of an object that already exists in the session
;; -- including one built at runtime, e.g. (make-cube 100 200 300).

(defvar *out*)
(defvar *movable-joints* nil)
(defvar *eus2urdf-model-path* nil)
(defvar *eus2urdf-constructor* nil)
(defvar *eus2urdf-out-path* nil)

(defun pj-str (s)
  ;; print a JSON string (with minimal escaping) to *out*
  (format *out* "\"")
  (let ((str (if (stringp s) s (format nil "~A" s))))
    (dotimes (i (length str))
      (let ((c (aref str i)))
        (cond ((= c #\") (format *out* "\\\""))
              ((= c #\\) (format *out* "\\\\"))
              (t (write-byte c *out*))))))
  (format *out* "\""))

(defun pj-num (v)
  ;; print a number as JSON, clamping +/-inf to +/-1e30 sentinels
  (cond ((not (numberp v)) (format *out* "null"))
        ((> v 1e30) (format *out* "1e30"))
        ((< v -1e30) (format *out* "-1e30"))
        (t (format *out* "~A" v))))

(defun pj-fvec (v)
  ;; float-vector / list of numbers -> JSON array
  (format *out* "[")
  (dotimes (i (length v))
    (when (> i 0) (format *out* ","))
    (pj-num (elt v i)))
  (format *out* "]"))

(defun pj-mat (m)
  ;; 2D array (rows x cols) -> JSON array of arrays
  (let ((nr (array-dimension m 0)))
    (format *out* "[")
    (dotimes (r nr)
      (when (> r 0) (format *out* ","))
      (pj-fvec (matrix-row m r)))
    (format *out* "]")))

(defun pj-ivec (v)
  ;; integer-vector -> JSON array
  (format *out* "[")
  (dotimes (i (length v))
    (when (> i 0) (format *out* ","))
    (format *out* "~A" (aref v i)))
  (format *out* "]"))

(defun pj-vertices (mat)
  ;; N x 3 vertex matrix -> JSON array of [x,y,z]
  (let ((nr (array-dimension mat 0)))
    (format *out* "[")
    (dotimes (r nr)
      (when (> r 0) (format *out* ","))
      (format *out* "[~A,~A,~A]"
              (aref mat r 0) (aref mat r 1) (aref mat r 2)))
    (format *out* "]")))

(defun joint-axis-fvec (j)
  ;; The axis slot may be a float-vector (urdfeus/euscollada models) or a
  ;; keyword like :z / :-z (classic jskeus sample models). Normalize to a
  ;; float-vector expressed in the child-link frame.
  (let ((ax (j . axis)))
    (cond
      ((derivedp ax float-vector) ax)
      ((listp ax) (coerce ax float-vector))
      ((keywordp ax)
       (case ax
         (:x (float-vector 1 0 0)) (:-x (float-vector -1 0 0))
         (:y (float-vector 0 1 0)) (:-y (float-vector 0 -1 0))
         (:z (float-vector 0 0 1)) (:-z (float-vector 0 0 -1))
         (t nil)))
      (t nil))))

(defun joint-type-name (j)
  (let ((cn (string-downcase (symbol-pname (send (class j) :name)))))
    cn))

(defun collect-all-links (root)
  (let ((acc nil))
    (labels ((rec (l)
               (push l acc)
               (dolist (c (send l :child-links)) (rec c))))
      (rec root))
    (reverse acc)))

(defun dump-mesh (glv)
  ;; dump one glvertices object's submeshes as JSON array
  (let ((ml (glv . gl::mesh-list))
        (gpos (send glv :worldpos))
        (grot (send glv :worldrot)))
    (format *out* "[")
    (let ((first t))
      (dolist (m ml)
        (let* ((mat (cadr (assoc :material m)))
               (ambient (cadr (assoc :ambient mat)))
               (diffuse (cadr (assoc :diffuse mat)))
               (indices (cadr (assoc :indices m)))
               (vertices (cadr (assoc :vertices m))))
          (when (and vertices (> (array-dimension vertices 0) 0))
            (unless first (format *out* ","))
            (setq first nil)
            (format *out* "{")
            (format *out* "\"ambient\":") (if ambient (pj-fvec ambient) (format *out* "null"))
            (format *out* ",\"diffuse\":") (if diffuse (pj-fvec diffuse) (format *out* "null"))
            (format *out* ",\"glv_pos\":") (pj-fvec gpos)
            (format *out* ",\"glv_rot\":") (pj-mat grot)
            (format *out* ",\"indices\":") (pj-ivec indices)
            (format *out* ",\"vertices\":") (pj-vertices vertices)
            (format *out* "}")))))
    (format *out* "]")))

(defun dump-plain-body (b)
  ;; Dump a non-collada body (e.g. make-cube) as a single world-coordinate
  ;; triangle-soup submesh. Faces are fan-triangulated; faces are assumed
  ;; convex (true for cubes / primitive shapes used as visualization aids).
  (send b :worldcoords)
  (let ((col (send b :get :face-color))
        (tris nil) (ntri 0))
    (dolist (f (send b :faces))
      (let* ((vs (send f :vertices)) (m (length vs)))
        ;; drop the closing vertex if it duplicates the first
        (when (and (> m 2)
                   (< (norm (v- (elt vs 0) (elt vs (1- m)))) 1e-4))
          (setq m (1- m)))
        (dotimes (i (- m 2))
          (push (elt vs 0) tris)
          (push (elt vs (+ i 1)) tris)
          (push (elt vs (+ i 2)) tris)
          (incf ntri))))
    (setq tris (reverse tris))
    (format *out* "[")
    (when tris
      (format *out* "{")
      (let ((c (if col (list (elt col 0) (elt col 1) (elt col 2) 1.0)
                   (list 0.7 0.7 0.7 1.0))))
        (format *out* "\"ambient\":") (pj-fvec c)
        (format *out* ",\"diffuse\":") (pj-fvec c))
      (format *out* ",\"glv_pos\":[0,0,0]")
      (format *out* ",\"glv_rot\":[[1,0,0],[0,1,0],[0,0,1]]")
      (format *out* ",\"indices\":[")
      (dotimes (i (* ntri 3)) (when (> i 0) (format *out* ",")) (format *out* "~A" i))
      (format *out* "]")
      (format *out* ",\"vertices\":[")
      (let ((vv tris) (first t))
        (dolist (v vv)
          (unless first (format *out* ","))
          (setq first nil)
          (format *out* "[~A,~A,~A]" (elt v 0) (elt v 1) (elt v 2))))
      (format *out* "]")
      (format *out* "}"))
    (format *out* "]")))

(defun dump-link (l &optional (prefix ""))
  (format *out* "{")
  (format *out* "\"name\":") (pj-str (format nil "~A~A" prefix (send l :name)))
  (format *out* ",\"parent\":")
  (if (send l :parent) (pj-str (format nil "~A~A" prefix (send (send l :parent) :name))) (format *out* "null"))
  (format *out* ",\"joint\":")
  (if (send l :joint) (pj-str (format nil "~A~A" prefix (send (send l :joint) :name))) (format *out* "null"))
  (format *out* ",\"pos\":") (pj-fvec (send l :worldpos))
  (format *out* ",\"rot\":") (pj-mat (send l :worldrot))
  ;; inertial
  (format *out* ",\"weight\":") (pj-num (send l :weight))
  (format *out* ",\"centroid\":")
  (let ((ac (l . acentroid)))
    (if ac (pj-fvec ac) (format *out* "null")))
  (format *out* ",\"inertia\":")
  (let ((it (send l :inertia-tensor)))
    (if it (pj-mat it) (format *out* "null")))
  ;; meshes: collada-body via glvertices, other bodies via their faces
  (format *out* ",\"meshes\":[")
  (let ((first t))
    (dolist (b (send l :bodies))
      (cond
        ((and (find-method b :glvertices) (send b :glvertices))
         (unless first (format *out* ","))
         (setq first nil)
         (dump-mesh (send b :glvertices)))
        ((and (not (find-method b :glvertices))
              (find-method b :faces) (send b :faces))
         (unless first (format *out* ","))
         (setq first nil)
         (dump-plain-body b)))))
  (format *out* "]")
  (format *out* "}"))

(defun dump-joint (j &optional (prefix ""))
  (format *out* "{")
  (format *out* "\"name\":") (pj-str (format nil "~A~A" prefix (send j :name)))
  (format *out* ",\"jtype\":") (pj-str (joint-type-name j))
  (format *out* ",\"movable\":") (format *out* "~A" (if (memq j *movable-joints*) "true" "false"))
  (format *out* ",\"axis\":")
  (let ((ax (joint-axis-fvec j))) (if ax (pj-fvec ax) (format *out* "null")))
  (format *out* ",\"min\":") (pj-num (send j :min-angle))
  (format *out* ",\"max\":") (pj-num (send j :max-angle))
  ;; achieved joint value at dump time (deg for rotational, mm for linear).
  ;; movable joints are zeroed but eus clamps to [min,max], so this may be
  ;; non-zero; it is backed out when computing the URDF joint-0 origin.
  (format *out* ",\"q\":") (pj-num (send j :joint-angle))
  (format *out* ",\"parent\":") (pj-str (format nil "~A~A" prefix (send (send j :parent-link) :name)))
  (format *out* ",\"child\":") (pj-str (format nil "~A~A" prefix (send (send j :child-link) :name)))
  (format *out* ",\"vel\":") (pj-num (send j :max-joint-velocity))
  (format *out* ",\"torque\":") (pj-num (send j :max-joint-torque))
  ;; mimic followers (eus stores followers on the master joint)
  (format *out* ",\"mimic\":")
  (let ((mj (when (assoc 'mimic-joints (send j :slots)) (j . mimic-joints))))
    (if mj
        (progn
          (format *out* "[")
          (let ((first t))
            (dolist (mp mj)
              (unless first (format *out* ","))
              (setq first nil)
              (format *out* "{\"joint\":") (pj-str (send (send mp :joint) :name))
              (format *out* ",\"multiplier\":") (pj-num (send mp :multiplier))
              (format *out* ",\"offset\":") (pj-num (send mp :offset))
              (format *out* "}")))
          (format *out* "]"))
        (format *out* "null")))
  (format *out* "}"))

(defun eus2urdf-instantiate ()
  ;; Resolve *eus2urdf-constructor* to a robot instance. It may name either a
  ;; constructor function (e.g. urdfeus/pr2eus models: (defun pr2 ...)) or a
  ;; robot class directly (e.g. jskeus sample models: (defclass h4-robot ...)
  ;; with no zero-arg constructor).
  (let ((sym (read-from-string *eus2urdf-constructor*)))
    (cond
      ((fboundp sym) (funcall sym))
      ((and (boundp sym) (classp (symbol-value sym)))
       (instance (symbol-value sym) :init))
      (t (error "'~A' is neither a function nor a class" sym)))))

(defun object-root (o)
  ;; topmost bodyset-link of an object (cascaded-link)
  (let ((root (car (send o :links))))
    (while (and (send root :parent) (derivedp (send root :parent) bodyset-link))
      (setq root (send root :parent)))
    root))

(defun dump-mount-joint (prefix root)
  ;; fixed joint mounting an object's root link to the synthetic "world" link
  (format *out* "{")
  (format *out* "\"name\":") (pj-str (format nil "~A~A" prefix "mount"))
  (format *out* ",\"jtype\":\"rotational-joint\",\"movable\":false,\"axis\":null")
  (format *out* ",\"min\":0,\"max\":0,\"q\":0")
  (format *out* ",\"parent\":\"world\",\"child\":")
  (pj-str (format nil "~A~A" prefix (send root :name)))
  (format *out* ",\"vel\":0,\"torque\":0,\"mimic\":null}"))

(defun dump-mount-named (prefix child-name)
  ;; fixed joint world -> a named (already-emitted) link
  (format *out* "{")
  (format *out* "\"name\":") (pj-str (format nil "~Amount" prefix))
  (format *out* ",\"jtype\":\"rotational-joint\",\"movable\":false,\"axis\":null")
  (format *out* ",\"min\":0,\"max\":0,\"q\":0,\"parent\":\"world\",\"child\":")
  (pj-str (format nil "~A~A" prefix child-name))
  (format *out* ",\"vel\":0,\"torque\":0,\"mimic\":null}"))

(defun dump-empty-link (name pos rot)
  ;; a geometry-less frame link (used for coordinate-only scene objects/spots)
  (format *out* "{\"name\":") (pj-str name)
  (format *out* ",\"parent\":null,\"joint\":null,\"pos\":") (pj-fvec pos)
  (format *out* ",\"rot\":") (pj-mat rot)
  (format *out* ",\"weight\":0,\"centroid\":null,\"inertia\":null,\"meshes\":[]}"))

(defun frame-parent-name (c prefix)
  ;; name (prefixed) of the link a coords frame is attached to, or null
  (let ((p (send c :parent)))
    (if (and p (find-method p :name) (send p :name))
        (pj-str (format nil "~A~A" prefix (send p :name)))
        (format *out* "null"))))

(defun dump-frame (c kind prefix)
  ;; one named coordinate frame (grasp handle / attention point) in WORLD pose
  (format *out* "{\"name\":") (pj-str (format nil "~A~A" prefix (send c :name)))
  (format *out* ",\"kind\":") (pj-str kind)
  (format *out* ",\"parent\":") (frame-parent-name c prefix)
  (format *out* ",\"pos\":") (pj-fvec (send c :worldpos))
  (format *out* ",\"rot\":") (pj-mat (send c :worldrot))
  (format *out* "}"))

(defun dump-frames-of (obj prefix firstp)
  ;; emit handle/attention frames of obj; returns updated first-flag.
  ;; eus exposes grasp poses via (send obj :handle) and attention points via
  ;; (send obj :attention) as lists of cascaded-coords.
  (let ((first firstp))
    (dolist (spec (list (cons :handle "handle") (cons :attention "attention")))
      (let ((m (car spec)) (kind (cdr spec)))
        (when (find-method obj m)
          (let ((cs (send obj m)))
            (when (listp cs)
              (dolist (c cs)
                (when (and c (find-method c :worldpos))
                  (unless first (format *out* ","))
                  (setq first nil)
                  (dump-frame c kind prefix))))))))
    first))

(defun scene-physical-p (o)
  ;; physical scene objects are cascaded-link (have links/joints); plain
  ;; cascaded-coords objects are coordinate-only spots (door-spot, etc).
  (and (derivedp o cascaded-link) (send o :links)))

(defun dump-robot (r)
  (let (root all-links all-joints)
    ;; zero all movable joints so link world transforms give URDF joint origins
    (dolist (j (send r :joint-list)) (send j :joint-angle 0))
    (send r :worldcoords)
    (setq *movable-joints* (send r :joint-list))
    ;; Walk up to the true root in case (car links) is not it: climb while the
    ;; parent is itself a link (bodyset-link); stop at the robot/cascoords root.
    (setq root (car (send r :links)))
    (while (and (send root :parent) (derivedp (send root :parent) bodyset-link))
      (setq root (send root :parent)))
    (setq all-links (collect-all-links root))
    ;; every non-root link contributes its parent joint (covers fixed joints too)
    (setq all-joints
          (remove nil (mapcar #'(lambda (l) (send l :joint)) all-links)))
    (setq *out* (open *eus2urdf-out-path* :direction :output))
    (format *out* "{")
    (format *out* "\"robot_name\":") (pj-str (send r :name))
    (format *out* ",\"root_link\":") (pj-str (send root :name))
    (format *out* ",\"links\":[")
    (let ((first t))
      (dolist (l all-links)
        (unless first (format *out* ","))
        (setq first nil)
        (dump-link l)))
    (format *out* "]")
    (format *out* ",\"joints\":[")
    (let ((first t))
      (dolist (j all-joints)
        (unless first (format *out* ","))
        (setq first nil)
        (dump-joint j)))
    (format *out* "]")
    (format *out* ",\"frames\":[")
    (dump-frames-of r "" t)
    (format *out* "]")
    (format *out* "}")
    (close *out*)))

(defun dump-scene (r)
  ;; A scene-model holds many objects (each a cascaded-link). Emit a single URDF
  ;; tree: a synthetic "world" root, every object's links/joints (name-prefixed
  ;; per object to avoid collisions), and a fixed mount world->object-root at the
  ;; object's world pose. Object-internal joints (doors/drawers) stay movable.
  (let ((objs (send r :objects)))
    ;; zero every physical object's movable joints
    (dolist (o objs)
      (when (scene-physical-p o)
        (dolist (j (send o :joint-list)) (send j :joint-angle 0))))
    (send r :worldcoords)
    (setq *movable-joints*
          (apply #'append
                 (mapcar #'(lambda (o) (if (scene-physical-p o)
                                           (send o :joint-list) nil)) objs)))
    (setq *out* (open *eus2urdf-out-path* :direction :output))
    (format *out* "{")
    (format *out* "\"robot_name\":") (pj-str (send r :name))
    (format *out* ",\"root_link\":\"world\"")
    (format *out* ",\"links\":[")
    ;; synthetic world root
    (format *out* "{\"name\":\"world\",\"parent\":null,\"joint\":null")
    (format *out* ",\"pos\":[0,0,0],\"rot\":[[1,0,0],[0,1,0],[0,0,1]]")
    (format *out* ",\"weight\":0,\"centroid\":null,\"inertia\":null,\"meshes\":[]}")
    (let ((i 0))
      (dolist (o objs)
        (let ((prefix (format nil "o~A_" i)))
          (if (scene-physical-p o)
              (dolist (l (collect-all-links (object-root o)))
                (format *out* ",") (dump-link l prefix))
              ;; coordinate-only spot -> single empty frame link
              (progn
                (format *out* ",")
                (dump-empty-link (format nil "~A~A" prefix (send o :name))
                                 (send o :worldpos) (send o :worldrot)))))
        (incf i)))
    (format *out* "]")
    (format *out* ",\"joints\":[")
    (let ((i 0) (first t))
      (dolist (o objs)
        (let ((prefix (format nil "o~A_" i)))
          (unless first (format *out* ","))
          (setq first nil)
          (if (scene-physical-p o)
              (let ((jts (remove nil (mapcar #'(lambda (l) (send l :joint))
                                             (collect-all-links (object-root o))))))
                (dump-mount-joint prefix (object-root o))
                (dolist (j jts)
                  (format *out* ",") (dump-joint j prefix)))
              (dump-mount-named prefix (send o :name))))
        (incf i)))
    (format *out* "]")
    (format *out* ",\"frames\":[")
    (let ((i 0) (first t))
      (dolist (o objs)
        (when (scene-physical-p o)
          (setq first (dump-frames-of o (format nil "o~A_" i) first)))
        (incf i)))
    (format *out* "]")
    ;; per-object name map (prefix -> object name) so a viewer can label objects
    (format *out* ",\"scene_objects\":[")
    (let ((i 0) (first t))
      (dolist (o objs)
        (unless first (format *out* ","))
        (setq first nil)
        (format *out* "{\"prefix\":") (pj-str (format nil "o~A_" i))
        (format *out* ",\"name\":") (pj-str (send o :name)) (format *out* "}")
        (incf i)))
    (format *out* "]")
    (format *out* "}")
    (close *out*)))

(defun body-meshes-of (obj)
  ;; "meshes" array for an object that is a body/bodyset rather than a link
  ;; tree. Mirrors the mesh half of dump-link.
  (let ((bs (if (find-method obj :bodies) (send obj :bodies) (list obj))))
    (format *out* "[")
    (let ((first t))
      (dolist (b bs)
        (cond
          ((and (find-method b :glvertices) (send b :glvertices))
           (unless first (format *out* ","))
           (setq first nil)
           (dump-mesh (send b :glvertices)))
          ((and (find-method b :faces) (send b :faces))
           (unless first (format *out* ","))
           (setq first nil)
           (dump-plain-body b)))))
    (format *out* "]")))

(defun dump-single-body (b)
  ;; A standalone body has no link tree (make-cube, make-cylinder, a mesh
  ;; loaded with load-mesh-file, ...). Emit it as a one-link model so it goes
  ;; through exactly the same URDF path as a robot.
  (let ((name (or (and (find-method b :name) (send b :name)) "body")))
    (setq *movable-joints* nil)
    (send b :worldcoords)
    (setq *out* (open *eus2urdf-out-path* :direction :output))
    (format *out* "{")
    (format *out* "\"robot_name\":") (pj-str name)
    (format *out* ",\"root_link\":") (pj-str name)
    (format *out* ",\"links\":[{\"name\":") (pj-str name)
    (format *out* ",\"parent\":null,\"joint\":null")
    (format *out* ",\"pos\":") (pj-fvec (send b :worldpos))
    (format *out* ",\"rot\":") (pj-mat (send b :worldrot))
    (format *out* ",\"weight\":0,\"centroid\":null,\"inertia\":null")
    (format *out* ",\"meshes\":") (body-meshes-of b)
    (format *out* "}]")
    (format *out* ",\"joints\":[],\"frames\":[]}")
    (close *out*)))

(defun eus2urdf-joint-list-of (o)
  ;; joint-list of o, or nil for objects that have no joints
  (if (find-method o :joint-list) (send o :joint-list) nil))

(defun eus2urdf-save-angles (o)
  ;; [(joint . angle) ...] for o and, for a scene-model, its member objects
  (let ((saved nil))
    (dolist (j (eus2urdf-joint-list-of o))
      (push (cons j (send j :joint-angle)) saved))
    (when (find-method o :objects)
      (dolist (c (send o :objects))
        (dolist (j (eus2urdf-joint-list-of c))
          (push (cons j (send j :joint-angle)) saved))))
    (nreverse saved)))

(defun eus2urdf-restore-angles (saved)
  (dolist (p saved) (send (car p) :joint-angle (cdr p))))

(defun eus2urdf-movable-joints (obj)
  "Movable joints of OBJ in the order dump-object-json emits them.

   This is the ordering contract between a dump and anything that records
   OBJ's joint angles over time: the i-th joint here is the i-th movable
   entry of the dumped \"joints\" array, and so the i-th movable joint of the
   URDF. Matching by index rather than by name matters because eus joint
   names are not unique -- a hand reusing :j10/:j11 on every finger is
   normal, and urdfeus renames such joints when it writes the URDF."
  (cond
    ((find-method obj :objects)
     (apply #'append
            (mapcar #'(lambda (o)
                        (if (scene-physical-p o)
                            (eus2urdf-movable-joints o)
                            nil))
                    (send obj :objects))))
    ((and (find-method obj :links) (send obj :links))
     (let ((jl (send obj :joint-list)))
       (remove-if-not
        #'(lambda (j) (memq j jl))
        (remove nil (mapcar #'(lambda (l) (send l :joint))
                            (collect-all-links (object-root obj)))))))
    (t nil)))

(defun eus2urdf-root-link (obj)
  "Link whose frame the URDF written for OBJ is expressed in.

   A dump bakes no world pose into the URDF, so this is the link a recorder
   must query to place the model: its worldcoords is the model's root pose."
  (cond
    ((find-method obj :objects) nil)  ;; scene: root is the synthetic world
    ((and (find-method obj :links) (send obj :links)) (object-root obj))
    (t obj)))

(defun dump-object-json (obj path)
  "Dump OBJ to PATH as eus2urdf JSON, then restore OBJ's joint angles.

   OBJ may be a robot/cascaded-link, a scene-model, or a plain body such as
   the result of (make-cube 100 200 300). Dumping zeroes every movable joint
   (that is what makes the link transforms usable as URDF joint origins), so
   the previous angles are put back before returning -- this is safe to call
   from inside a running motion."
  (let ((prev-out *eus2urdf-out-path*)
        (saved (eus2urdf-save-angles obj)))
    (setq *eus2urdf-out-path* path)
    (unwind-protect
        (cond
          ((find-method obj :objects) (dump-scene obj))
          ((and (find-method obj :links) (send obj :links)) (dump-robot obj))
          (t (dump-single-body obj)))
      (eus2urdf-restore-angles saved)
      (send obj :worldcoords)
      (setq *eus2urdf-out-path* prev-out)))
  path)

(defun eus2urdf-dump ()
  (let ((r (eus2urdf-instantiate)))
    (if (find-method r :objects)
        (dump-scene r)
        (dump-robot r))))

;; Only run the file+constructor conversion when a model path was supplied.
;; Without it this file is a library: (load) it and call dump-object-json.
(when *eus2urdf-model-path*
  (load *eus2urdf-model-path*)
  (eus2urdf-dump)
  (exit))
