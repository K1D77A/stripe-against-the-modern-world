;;;; stripe-against-the-modern-world.lisp

(in-package #:stripe-against-the-modern-world)

(defparameter *api-version* "2020-08-27")



(defparameter *url* "https://api.stripe.com")


(defapi%get balance%get-balance ("/v1/balance"))

(defapi%get balance-transactions%all ("/v1/balance_transactions"))

(defapi%get balance-transactions%id ("/v1/balance_transactions/:id"))



(defapi charges%create ("/v1/charges" post-request))

(defapi%get charges%all ("/v1/charges"))

(defapi%get charges%id ("/v1/charges/:id"))

(defapi charges%update-id ("/v1/charges/:id" post-request))

(defapi charges%capture-id ("/v1/charges/:id/capture" post-request))



(defapi customers%create ("/v1/customers" post-request))

(defapi customers%update-id ("/v1/customers/:id" post-request))

(defapi%get customers%all ("/v1/customers"))

(defapi%get customers%id ("/v1/customers/:id"))

(defapi%delete customers%id ("/v1/customers/:id"))



(defapi%get disputes%all ("/v1/disputes"))

(defapi%get disputes%id ("/v1/disputes/:id"))

(defapi disputes%update-id ("/v1/disputes/:id" post-request))

(defapi disputes%close-id ("/v1/disputes/:id/close" post-request))



(defapi%get events%all ("/v1/events"))

(defapi%get events%id ("/v1/events/:id"))



(defapi%get files%all ("/v1/files"))

(defapi%get files%id ("/v1/files/:id"))

(defapi files%create ("/v1/files" post-files-request))


(defapi%get file_links%id ("/v1/files_links/:id"))

(defapi%get file_links%all ("/v1/files_links"))

(defapi file_links%update-id ("/v1/files_links/:id" post-request))

(defapi file_links%create ("/v1/files_links" post-request))












