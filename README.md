tunebook
========

I wanted something that could produce ad-hoc scores for a bunch of tunes to play with friends.  This is pretty simple.  Put the ABC notation files of the tunes you want to share in a directory somewhere, run the app in the browser, navigate to the directory and select the tunes you want to share.  Press `print scores` in order to produce your off-the-cuff tunebook.

This needs to be run in the browser (and not from the command line) because the score production (abc-scores) only runs in the browser.

Try it out [here](http://www.tradtunedb.org.uk:8606/).

Building
--------

from the current directory:

    $ npm run build

Building the example
--------------------

from the current directory:

    $ npm run example   

Then navigate to example/dist/index.html.
