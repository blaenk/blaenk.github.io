This is the source for my [personal site](http://blaenkdenum.com).

TODO:

* implement a preview system
  * possibilities:
    * somehow mark that there are preview posts, and if so, regenerate site without them
    * hack into Snap server
    * don't generate post, only serve copy from separate place. won't show up anywhere else


* make abbreviation filter use blaze html instead of strings
* make the abbreviation filter into a pandoc transformer?
  * collect abbreviations in its pass
  * substitute abbreviations in pandoc transformer?
