;;;; stripe-against-the-modern-world.asd

(asdf:defsystem #:stripe-against-the-modern-world
                :description "Implementation of the Stripe API."
                :author "K1D77A"
                :license  "MIT"
                :version "0.0.1"
                :serial t
                :pathname "api"
                :depends-on (#:str
                             #:dexador
                             #:jonathan
                             #:alexandria
                             #:ironclad
                             #:local-time
                             #:ningle
                             #:do-urlencode
                             #:closer-mop)
                :components ((:file "package")
                             (:file "conditions")
                             (:file "protocol")
                             (:file "helpers")
                             (:file "webhooks")
                             (:file "stripe-against-the-modern-world")))

