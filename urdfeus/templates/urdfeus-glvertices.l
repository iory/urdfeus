;;
;; urdfeus-glvertices : gl::glvertices that paints GL_FRONT_AND_BACK
;;
;; Upstream irtgl.l assigns a mesh :material only to GL_BACK.  Together with
;; glFrontFace(GL_CW) (glview.l) and two-sided lighting, that means any
;; triangle presenting its GL_FRONT side is drawn with OpenGL's *default*
;; front material -- neutral grey -- instead of the mesh colour.  Closed
;; solids hide those faces inside, but the zero-thickness single-sided sheets
;; that CAD exporters emit (laser fans, decals, labels) show grey wedges.
;;
;; This subclass overrides only :draw, so every other gl::glvertices in the
;; session keeps the stock code path.
;;
;; :draw below is copied verbatim from jskeus 1.2.5 irteus/irtgl.l with the
;; material face changed from GL_BACK to GL_FRONT_AND_BACK.  Re-sync it if
;; jskeus changes :draw upstream; drop this file entirely once jskeus paints
;; GL_FRONT_AND_BACK itself.
;;
;; ---------------------------------------------------------------------------
;; The :draw method is derived from jskeus, distributed under the following
;; terms:
;;
;;;
;;; Copyright (c) 1987- JSK, The University of Tokyo.  All Rights Reserved.
;;;
;;; This software is a collection of EusLisp code for robot applications,
;;; which has been developed by the JSK Laboratory for the IRT project.
;;; For more information on EusLisp and its application to the robotics,
;;; please refer to the following papers.
;;;
;;; Toshihiro Matsui
;;; Multithread object-oriented language euslisp for parallel and
;;;  asynchronous programming in robotics
;;; Workshop on Concurrent Object-based Systems,
;;;  IEEE 6th Symposium on Parallel and Distributed Processing, 1994
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions are met:
;;;
;;; * Redistributions of source code must retain the above copyright notice,
;;;   this list of conditions and the following disclaimer.
;;; * Redistributions in binary form must reproduce the above copyright notice,
;;;   this list of conditions and the following disclaimer in the documentation
;;;   and/or other materials provided with the distribution.
;;; * Neither the name of JSK Robotics Laboratory, The University of Tokyo
;;;   (JSK) nor the names of its contributors may be used to endorse or promote
;;;   products derived from this software without specific prior written
;;;   permission.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
;;; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
;;; THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
;;; PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR
;;; CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
;;; EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
;;; PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS;
;;; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
;;; OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
;;; ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;; -------------------------------------------------------------------------

(in-package "GL")

(unless (boundp 'urdfeus-glvertices)
  (defclass urdfeus-glvertices :super glvertices :slots ())
  (defmethod urdfeus-glvertices
  (:draw (vwr &rest args)
   (let* (newlis
          (mat (send (send self :worldcoords) :4x4))
          (stransparent (get self :transparent))
          fcol
#+:jsk
          (glcon (cdr (assq (sys:thread-self) ((send vwr :viewsurface) . glcon))))
#-:jsk
          (glcon ((send vwr :viewsurface) . glcon))
          )
#+:jsk
     (sys::mutex-lock gl::*opengl-lock*)
     (send vwr :viewsurface :makecurrent) ;; ???
     (glPushAttrib GL_ALL_ATTRIB_BITS)
     (glpushmatrix)
     (glmultmatrixf (array-entity (transpose mat *temp-matrix*)))
     (gl::glLightModeli GL_LIGHT_MODEL_TWO_SIDE GL_TRUE) ;; draw back side
     (cond
      ((setq newlis (cdr (assq glcon (get self :GL-DISPLAYLIST-ID))))
       (glCallList newlis))
      (t
       ;; search face color
       (setq fcol (get self :face-color))
       (when (and fcol (not (vectorp fcol)))
         ;;(warn "draw-body: body ~A face-color ~A~%" abody col)
         (setq fcol (find-color fcol))
         (setf (get self :face-color) fcol))

       (setq newlis (glgenlists 1))
       (glnewlist newlis gl_compile)

       (when stransparent
         (glDepthMask GL_FALSE)
         (glEnable GL_BLEND)
         (glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA))

       (dolist (minfo mesh-list)
         (let ((mesh-type (cadr (assoc :type minfo)))
               (material-info (cadr (assoc :material minfo)))
               (vertices (cadr (assoc :vertices minfo)))
               (normals (cadr (assoc :normals minfo)))
               (indices (cadr (assoc :indices minfo)))
               (texcoords (cadr (assoc :texcoords minfo)))
               (flat-shade (cadr (assoc :flat minfo)))
               teximg)
           (when flat-shade
             (glEnable GL_FLAT)
             (glShadeModel GL_FLAT))
           (cond
            (fcol ;; use :face-color
             (glColor3fv fcol)
             (cond
              (stransparent
               (glMaterialfv GL_BACK
                             GL_AMBIENT_AND_DIFFUSE
                             (concatenate float-vector fcol (float-vector stransparent)))
               (glMaterialfv GL_FRONT
                             GL_AMBIENT_AND_DIFFUSE (float-vector 0 0 0 0)))
              (t
               (glMaterialfv GL_BACK  GL_AMBIENT_AND_DIFFUSE fcol)
               (glMaterialfv GL_FRONT GL_AMBIENT_AND_DIFFUSE (float-vector 0.8 0.0 0.54))
               ))
             ;;(glMaterialfv GL_FRONT_AND_BACK GL_SPECULAR (float-vector 0.2 0.2 0.2 0.1))
             ;;(glMaterialfv GL_FRONT_AND_BACK GL_EMISSION (float-vector 0.1 0.1 0.1 0.1))
             )
            (material-info
             (let (;;(fnm   (cadr (assoc :filename material-info)))
                   (col   (cadr (assoc :color material-info)))
                   (amb   (cadr (assoc :ambient material-info)))
                   (diff  (cadr (assoc :diffuse material-info)))
                   (spec  (cadr (assoc :specular material-info)))
                   (emi   (cadr (assoc :emission material-info)))
                   (shin  (cadr (assoc :shininess material-info)))
                   (trans (cadr (assoc :transparency material-info))))
               (setq teximg (cadr (assoc :teximage material-info)))
               ;; override transparent ...
               ;;(if (and stransparent trans) (setq stransparent trans))
               (when col
                 (glColor3fv col)
                 (glMaterialfv GL_FRONT_AND_BACK GL_AMBIENT_AND_DIFFUSE
                               (if stransparent
                                   (concatenate float-vector col (float-vector stransparent))
                                 col)))
               (when amb
                 (if stransparent
                     (setf (elt amb 3) stransparent))
                 (glMaterialfv GL_FRONT_AND_BACK GL_AMBIENT amb))
               (when diff
                 (if stransparent
                     (setf (elt diff 3) stransparent))
                 (glMaterialfv GL_FRONT_AND_BACK GL_DIFFUSE diff))
               (when spec
                 (if stransparent
                     (setf (elt spec 3) stransparent))
                 (glMaterialfv GL_FRONT_AND_BACK GL_SPECULAR spec))
               (when emi
                 (if stransparent
                     (setf (elt emi 3) stransparent))
                 (glMaterialfv GL_FRONT_AND_BACK GL_EMISSION emi))
               (cond
                (shin
                 (glMaterialf GL_FRONT_AND_BACK GL_SHININESS shin))
                (t
                 (glMaterialf GL_FRONT_AND_BACK GL_SHININESS 0.0)
                 (glMaterialfv GL_FRONT_AND_BACK GL_SPECULAR (float-vector 0 0 0 0))
                 ))
               (when (and teximg texcoords)
                 (let ((im_width  (send teximg :width))
                       (im_height (send teximg :height))
                       (im_buf    (send teximg :entity)))
                   (glPixelStorei GL_UNPACK_ALIGNMENT 1)
                   ;;
                   (cond
                    ((derivedp teximg image::grayscale-image)
                     (glTexImage2D GL_TEXTURE_2D 0 GL_RGB im_width im_height
                                   0 GL_LUMINANCE GL_UNSIGNED_BYTE im_buf))
                    ((derivedp teximg image::color-image24)
                     (glTexImage2D GL_TEXTURE_2D 0 GL_RGB im_width im_height
                                   0 GL_RGB GL_UNSIGNED_BYTE im_buf))
                    ((derivedp teximg image::color-image32)
                     (glTexImage2D GL_TEXTURE_2D 0 GL_RGBA im_width im_height
                                   0 GL_RGBA GL_UNSIGNED_BYTE im_buf)))
                   ;;(gluBuild2DMipmaps GL_TEXTURE_2D GL_RGB im_width im_height GL_RGB GL_UNSIGNED_BYTE imbuf) ;; case 2
                   ;;
                   (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S GL_REPEAT)
                   (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T GL_REPEAT)
                   ;;
                   (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_NEAREST)
                   (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_NEAREST)
                   ;;(glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_LINEAR) ;; case 2
                   ;;(glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR_MIPMAP_LINEAR) ;; case 2

                   ;;(glTexEnvi GL_TEXTURE_ENV GL_TEXTURE_ENV_MODE GL_DECAL) ;; flat color
                   (glTexEnvi GL_TEXTURE_ENV GL_TEXTURE_ENV_MODE GL_MODULATE)
                   (glEnable GL_TEXTURE_2D)
                   ))
               (when stransparent
                 (glMaterialfv GL_FRONT
                               GL_AMBIENT_AND_DIFFUSE (float-vector 0 0 0 0)))
               ))
            (t ;; default color
             ;; Font face was set CW at glview.l (glFrontFace GL_CW)
             (glMaterialfv GL_BACK  GL_AMBIENT (float-vector 0.2 0.2 0.2 0.1))
             (glMaterialfv GL_BACK  GL_DIFFUSE (float-vector 1 1 1 1))
             (glMaterialfv GL_BACK  GL_SPECULAR (float-vector 0.2 0.2 0.2 0.1))
             (glMaterialfv GL_BACK  GL_EMISSION (float-vector 0.1 0.1 0.1 0.1))
             ;;
             (glMaterialfv GL_FRONT GL_AMBIENT (float-vector 0.8 0.0 0.54 1))
             (glMaterialfv GL_FRONT GL_DIFFUSE (float-vector 0.8 0.0 0.54 1))
             (glMaterialfv GL_FRONT GL_SPECULAR (float-vector 0.2 0.2 0.2 1))
             (glMaterialfv GL_FRONT GL_EMISSION (float-vector 0.1 0.1 0.1 1))
             )
            ) ;; /cond
           (unless (and teximg texcoords) (setq texcoords nil))
           ;;
           (glEnableClientState GL_VERTEX_ARRAY)
#+(or :x86_64 :aarch64)
           (glVertexPointer 3 GL_DOUBLE 0 (array-entity vertices))
#-(or :x86_64 :aarch64)
           (glVertexPointer 3 GL_FLOAT 0 (array-entity vertices))
           ;;
           (when normals
             (glEnableClientState GL_NORMAL_ARRAY)
#+(or :x86_64 :aarch64)
             (glNormalPointer GL_DOUBLE 0 (array-entity normals))
#-(or :x86_64 :aarch64)
             (glNormalPointer GL_FLOAT 0 (array-entity normals))
             )
           ;;
           (when texcoords
             (glEnableClientState GL_TEXTURE_COORD_ARRAY)
#+(or :x86_64 :aarch64)
             (glTexCoordPointer 2 GL_DOUBLE 0 texcoords)
#-(or :x86_64 :aarch64)
             (glTexCoordPointer 2 GL_FLOAT 0 texcoords)
             )

           (let (tp)
             (case mesh-type
               (:triangles (setq tp GL_TRIANGLES))
               (:quads     (setq tp GL_QUADS))
               (:lines     (setq tp GL_LINES))
               (nil (setq tp GL_TRIANGLES) (warn ";; keyword :type not found (processing as :triangles)~%"))
               (t (warn ";; mesh-type not found ~A~%") mesh-type))
             (when tp
               (cond
                (indices
#+(or :x86_64 :aarch64)
                 (glDrawElements tp (length indices) GL_UNSIGNED_INT
                                 (user::lvector2integer-bytestring indices))
#-(or :x86_64 :aarch64)
                 (glDrawElements tp (length indices) GL_UNSIGNED_INT indices)
                 )
                (t
                 (glDrawArrays tp 0 (array-dimension vertices 0)))
                )))

           (when texcoords
             (glDisable GL_TEXTURE_2D)
             (glDisableClientState GL_TEXTURE_COORD_ARRAY))
           (when normals (glDisableClientState GL_NORMAL_ARRAY))
           (glDisableClientState GL_VERTEX_ARRAY)
           (when flat-shade
             (glEnable GL_SMOOTH)
             (glShadeModel GL_SMOOTH))
           ))

       (when stransparent
         (glDepthMask GL_TRUE)
         (glDisable GL_BLEND))

       (glendlist)
       (setf (get self :GL-DISPLAYLIST-ID)
             (cons (cons glcon newlis)
                   (get self :GL-DISPLAYLIST-ID)))
       (setq newlis nil)
       ))
     (gl::glLightModeli GL_LIGHT_MODEL_TWO_SIDE GL_TRUE) ;; return to default
     (glpopmatrix)
     (glpopattrib)
#+:jsk
     (sys::mutex-unlock *opengl-lock*)
     (unless newlis (send self :draw vwr))
     ))
  (:collision-check-objects (&rest args)) ;; compatibility to jsk library
  (:box ()
   (unless bbox
     (send self :calc-bounding-box))
   bbox)
  (:make-pqpmodel (&key (fat 0))
   (let ((m (geo::pqpmakemodel))
         v1 v2 v3 (id 0))
     (dolist (mesh mesh-list) ;; check triangle model
       (let ((tp (cadr (assoc :type mesh))))
         (unless (eq tp :triangles)
           (warn ";; not supported mesh type -> ~A" tp)
           (geo::pqpdeletemodel m)
           (return-from :make-pqpmodel nil))))
     (setq v1 (float-vector 0 0 0))
     (setq v2 (float-vector 0 0 0))
     (setq v3 (float-vector 0 0 0))
     (geo::pqpbeginmodel m)
     (dolist (mesh mesh-list)
       (let* ((vlst (cadr (assoc :vertices mesh)))
              (idces (cadr (assoc :indices mesh)))
              (idlen (* (/ (length idces) 3) 3)))
         (cond
           (idces
            (do ((v 0 (+ v 3)))
                ((>= v idlen))
              (user::c-matrix-row vlst (elt idces    v   ) v1)
              (user::c-matrix-row vlst (elt idces (+ v 1)) v2)
              (user::c-matrix-row vlst (elt idces (+ v 2)) v3)
              (when (not (= fat 0))
                (v+ v1 (scale fat (normalize-vector v1)) v1)
                (v+ v2 (scale fat (normalize-vector v2)) v2)
                (v+ v3 (scale fat (normalize-vector v3)) v3))
              (geo::pqpaddtri m v1 v2 v3 id)
              (incf id)
              ))
           (t
            (do ((v 0 (+ v 3)))
                ((>= v (array-dimension vlst 0)))
              (user::c-matrix-row vlst    v    v1)
              (user::c-matrix-row vlst (+ v 1) v2)
              (user::c-matrix-row vlst (+ v 2) v3)
              (when (not (= fat 0))
                (v+ v1 (scale fat (normalize-vector v1)) v1)
                (v+ v2 (scale fat (normalize-vector v2)) v2)
                (v+ v3 (scale fat (normalize-vector v3)) v3))
              (geo::pqpaddtri m v1 v2 v3 id)
              (incf id)
              )))
         ))
     (geo::pqpendmodel m)
     m
     ))
    ))

(in-package "USER")
