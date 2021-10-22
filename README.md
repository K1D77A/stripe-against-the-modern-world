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


## License

MIT

