(in-package #:stripe-against-the-modern-world)

(defclass api-call (c2mop:funcallable-standard-class)
  ((string-constructor
    :accessor string-constructor
    :initform nil)))

(defclass api-slot (c2mop:slot-definition)
  ((translator 
    :accessor translator 
    :initarg :translator 
    :initform nil)))

(defclass api-slot-direct (api-slot c2mop:standard-direct-slot-definition)
  ())

(defclass api-slot-effective (api-slot c2mop:standard-effective-slot-definition)
  ())


(defmethod c2mop:validate-superclass ((class api-call)
                                      (metaclass c2mop:funcallable-standard-class))
  t)

(defmethod c2mop:validate-superclass ((class api-slot)
                                      (metaclass standard-class))
  t)


(defmethod c2mop:effective-slot-definition-class ((class api-call) &rest initargs)
  (find-class 'api-slot-effective))

(defmethod c2mop:direct-slot-definition-class ((class api-call) &rest initargs)
  (find-class 'api-slot-direct))

(defmethod c2mop:compute-effective-slot-definition ((class api-call) name dslots)
  (call-next-method))

(defmethod c2mop:compute-slots ((class api-call))
  (if (eql (class-name class) 'api)
      (call-next-method)
      (with-slots (string-constructor endpoint required-slots special-slot)
          class        
        (setf required-slots (remove-if-not #'requiredp (c2mop:class-direct-slots class))
              special-slot (find-special-slot class))
        (call-next-method))))

