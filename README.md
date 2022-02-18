# stripe-against-the-modern-world

This is a an implementation of the Stripe API. 

Currently have wrapped the section Core Resources and under Products the products, prices and shipping then, under Checkout the sessions and finally under Webhooks the webhooks. 

## This is a WIP but will be used in a production environment so expect maintenance




## How to 
To change the default parser from jojo's plist to a hash-table change `*parse-as*` to 
a valid (jojo:parse <content> :as <key>), I suggest :hash-table


First you have to set `*api-key*` to your api key from stripe, you can do this lexically ofcourse. Best run a few tests, so use your test keys first.

Then you simply do the following:

```lisp 
SATMW> (make-instance 'events%all)
#<EVENTS%ALL {100F16210B}>
SATMW> (call-api *)
(:|url| "/v1/events" :|has_more| NIL :|data| NIL :|object| "list")
```
Jonathan is used for parsing. 
Any API error is caught and converted into a condition as per the Stripe documentation. 

If you have a call that requires an argument like an `:id` in the path then there will be a slot by that name which you fill on creation.

```lisp
SATMW> (make-instance 'events%id :id "abc")
#<EVENTS%ID {100F182A7B}>
SATMW> (call-api *)
<invalid-request-error because no known id>
```
If you have a post request that requires values then these requests have a slot called `content` that you fill with an ALIST.
```lisp
SATMW> (make-instance 'charges%create :content '(("amount" . 100)("currency" . "gbp")("source" . "abc")))
#<CHARGES%CREATE {100F4CF67B}>
```
Dexador is used to send the requests so it must be a properly formed ALIST.

## Alist construct
In `src/helpers.lisp` I have built a very simple DSL which will parse into an alist, you can pass the result of evaluating this as the :content key to dex:post. 
```lisp
(defparameter *test* 
  '(("fur" . "fluffy")
    ("cat" . "dog")
    (:array "woofers"
     ("dog" "wolf")
     (("smol" . "shih-tzu")
      ("big" . "labrador")))
    (:array "animals"
     (("oof" . "doof")
      ("kaboof" . "foo"))
     ("dog"
      "cat"
      "bird"))
    (:array "images"
     (("fur" . "fluffy")
      ("colour" . "brown")))
    ("fur" . "fluffy")
    ("colour" . "brown")))

SATMW> (ec *test*)
(("fur" . "fluffy") ("cat" . "dog") ("woofers[0]" . "dog")
 ("woofers[1]" . "wolf") ("woofers[2][smol]" . "shih-tzu")
 ("woofers[2][big]" . "labrador") ("animals[0][oof]" . "doof")
 ("animals[0][kaboof]" . "foo") ("animals[1]" . "dog") ("animals[2]" . "cat")
 ("animals[3]" . "bird") ("images[0][fur]" . "fluffy")
 ("images[0][colour]" . "brown") ("fur" . "fluffy") ("colour" . "brown"))
 ```
 It accepts an arbitrary number of lists and appends them together. 
 The DSL means you can create an alist that will correctly format as a form-url encoded string, this is annoying but its how Stripe handles requests...
 
Supports nested arrays although I've never tested it.
```lisp
(defparameter *test2* 
  '(("fur" . "fluffy")
    ("cat" . "dog")
    (:array "animals"
     (("oof" . "doof")
      ("kaboof" . "foo"))
     ("dog"
      "cat"
      "bird"))
    (:array "images"
     (("fur" . "fluffy")
      ("colour" . "brown"))
     (:array "nested-images"
      (("fluff" . "fluffy"))
      ("pos" "foo" "bar")))
    (:array "cats"
     ("brown" "white" "black"))
    ("fur" . "fluffy")
    ("colour" . "brown")))

SATMW> (ec *test2*)
(("fur" . "fluffy") ("cat" . "dog") ("animals[0][oof]" . "doof")
 ("animals[0][kaboof]" . "foo") ("animals[1]" . "dog") ("animals[2]" . "cat")
 ("animals[3]" . "bird") ("images[0][fur]" . "fluffy")
 ("images[0][colour]" . "brown") ("images[1][0][fluff]" . "fluffy")
 ("images[1][1]" . "pos") ("images[1][2]" . "foo") ("images[1][3]" . "bar")
 ("cats[0]" . "brown") ("cats[1]" . "white") ("cats[2]" . "black")
 ("fur" . "fluffy") ("colour" . "brown"))
```
`ec` now also accepts hash-tables and will attempt to convert them into the correctly encoded format for Stripe. You can even combine lists written in the basic DSL I wrote with hash-tables to produce one large alist to pass to Stripe.
## Webhooks

To verify the webhooks from Stripe you need to follow the instructions here:
https://stripe.com/docs/webhooks/signatures

Extract the raw-body, the signature (v1), and the timestamp then 
pass them as arguments to `verify-signature`. This returns a boolean (t or nil) 
to tell you if it validated and the time difference between the timestamp received 
and `local-time:now`

There is currently one build in method to validate instances `lack.request:request`
these are the wrappers created by Ningle (which uses clack and lack), so you can `verify-webhook` with `ningle:*request*` and your signing secret. See `./api/webhooks.lisp` to see how to implement verification for other servers.

An example of `verify-webhook` with Ningle:
```lisp
(setf (ningle/app:route *app* *stripe-webhook* :method :post)
      (lambda (params)
        (declare (ignore params))
        (multiple-value-bind (validp time-dif raw)
            (satmw:verify-webhook *stripe-webhook-signing-secret* ningle:*request*)
          (if (validate-webhook :stripe validp time-dif)
              "fail"
              (let* ((parsed (jojo:parse (babel:octets-to-string raw)
                                         :as :hash-table)))
                (process-webhook :stripe parsed))))))
```


## License

MIT

