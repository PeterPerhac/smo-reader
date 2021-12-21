# SMO reader

SMO files are archived SMS messages, generated by Siemens mobile phones. Those files are created when moving SMS message to the phone archive folder. SMO files are binary encoded and as such cannot be viewed with common text viewer. This is an application able to read the contents of SMO files.

The above is a cut-down version of what I found here

http://sisms.sourceforge.net/docs/SMISMOStruct.html

I am using the above page to figure out how to read the files.
Motivation for this project is simple, I found a whole bunch of 20 year old SMO files and I am curious what those files contain. To read the message inside them is actually quite easy. `cat` command gets you a long way... But I want to read the whole thing - the metadata as well as the message payload.


Input - binary SMO file:

```
00000000  0b 0b 01 0b 00 03 03 03  04 00 00 00 00 00 00 00  |................|
00000010  07 07 91 24 91 50 00 30  30 51 00 0a 81 90 50 04  |...$.P.00Q....P.|
00000020  96 51 00 08 ff 8b 06 08  04 cc f5 03 01 00 4b 00  |.Q............K.|
00000030  79 00 6d 00 20 00 64 00  6f 00 68 00 6f 00 72 00  |y.m. .d.o.h.o.r.|
00000040  69 00 20 00 70 00 72 00  73 00 6b 00 61 00 76 00  |i. .p.r.s.k.a.v.|
00000050  6b 00 61 00 20 00 6e 00  61 00 20 00 74 00 72 00  |k.a. .n.a. .t.r.|
00000060  61 00 76 00 65 00 20 00  61 00 20 00 76 00 6c 00  |a.v.e. .a. .v.l.|
00000070  61 00 73 00 79 00 20 00  64 00 6f 00 74 00 6c 00  |a.s.y. .d.o.t.l.|
00000080  65 00 6a 00 75 00 20 00  6e 00 61 00 20 00 68 00  |e.j.u. .n.a. .h.|
00000090  6c 00 61 00 76 00 65 00  20 00 2c 00 20 00 6b 00  |l.a.v.e. .,. .k.|
000000a0  79 00 6d 00 20 00 62 00  75 00 64 00 65 00 73 00  |y.m. .b.u.d.e.s.|
000000b0  20 ff ff ff ff ff ff ff  ff ff ff ff ff ff ff ff  | ...............|
000000c0  07 07 91 24 91 50 00 30  30 51 00 0a 81 90 50 04  |...$.P.00Q....P.|
000000d0  96 51 00 08 ff 8b 06 08  04 cc f5 03 02 00 70 00  |.Q............p.|
000000e0  6f 00 63 00 75 00 74 00  20 00 7a 00 76 00 75 00  |o.c.u.t. .z.v.u.|
000000f0  6b 00 79 00 20 00 76 00  20 00 75 00 63 00 68 00  |k.y. .v. .u.c.h.|
00000100  75 00 20 00 70 00 6f 00  20 00 73 00 75 00 70 00  |u. .p.o. .s.u.p.|
00000110  65 00 72 00 20 00 72 00  61 00 6e 00 65 00 20 00  |e.r. .r.a.n.e. .|
00000120  64 00 65 00 6c 00 6f 00  62 00 75 00 63 00 68 00  |d.e.l.o.b.u.c.h.|
00000130  75 00 20 00 61 00 20 00  6f 00 64 00 73 00 74 00  |u. .a. .o.d.s.t.|
00000140  72 00 61 00 6e 00 69 00  73 00 20 00 63 00 72 00  |r.a.n.i.s. .c.r.|
00000150  65 00 70 00 79 00 20 00  76 00 20 00 6f 00 6b 00  |e.p.y. .v. .o.k.|
00000160  75 ff ff ff ff ff ff ff  ff ff ff ff ff ff ff ff  |u...............|
00000170  07 07 91 24 91 50 00 30  30 51 00 0a 81 90 50 04  |...$.P.00Q....P.|
00000180  96 51 00 08 ff 3f 06 08  04 cc f5 03 03 00 2c 00  |.Q...?........,.|
00000190  20 00 76 00 73 00 65 00  74 00 6b 00 6f 00 20 00  | .v.s.e.t.k.o. .|
000001a0  64 00 6f 00 62 00 72 00  65 00 20 00 76 00 20 00  |d.o.b.r.e. .v. .|
000001b0  4e 00 6f 00 76 00 6f 00  6d 00 20 00 72 00 6f 00  |N.o.v.o.m. .r.o.|
000001c0  6b 00 75 00 21 ff ff ff  ff ff ff ff ff ff ff ff  |k.u.!...........|
000001d0  ff ff ff ff ff ff ff ff  ff ff ff ff ff ff ff ff  |................|
000001e0  ff ff ff ff ff ff ff ff  ff ff ff ff ff ff ff ff  |................|
000001f0  ff ff ff ff ff ff ff ff  ff ff ff ff ff ff ff ff  |................|
00000200  ff ff ff ff ff ff ff ff  ff ff ff ff ff ff ff ff  |................|
00000210  ff ff ff ff ff ff ff ff  ff ff ff ff ff ff ff ff  |................|
```

Output:

```
File: /Users/peterperhac/my/scala/smo-reader/my-smos/13_01_2004_20_14_12_MobileMSG73.smo
	From = 0905406915
	3 of 3 parts
	SMS Type = Outgoing, Status = Unsent
	Message =
Kym dohori prskavka na trave a vlasy dotleju na hlave , kym budes pocut zvuky v uchu po super rane delobuchu a odstranis crepy v oku, vsetko dobre v Novom roku!
```

