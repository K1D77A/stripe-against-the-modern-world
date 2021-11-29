# stripe-against-the-modern-world

This is a an implementation of the Stripe API. 

Currently have wrapped the section Core Resources.

## This is a WIP but will be used in a production environment so expect maintenance

## How to 
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


## License

MIT

