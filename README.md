# SMO reader

SMO files are archived SMS messages, generated by Siemens mobile phones. Those files are created when moving SMS message to the phone archive folder. SMO files are binary encoded and as such cannot be viewed with common text viewer. This is an application able to read the contents of SMO files.

The above is a cut-down version of what I found here

http://sisms.sourceforge.net/docs/SMISMOStruct.html

I am using the above page to figure out how to read the files.
Motivation for this project is simple, I found a whole bunch of 20 year old SMO files and I am curious what those files contain. To read the message inside them is actually quite easy. `cat` command gets you a long way... But I want to read the whole thing - the metadata as well as the message payload.
