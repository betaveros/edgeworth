edgeworth
=========

An online interface for constructing and solving grid-based puzzles.

`edgeworth` somewhat evolved from `gridderface`, this Scala Swing GUI program that I use to create and mark puzzles. Both of these projects were largely borne of a desire to make it easy to draw and mark grids, particularly edges, especially with the keyboard. Existing spreadsheets, applications, and even flood filling in an image editor are fairly conducive to solving puzzles where you fill in cells --- often you can even get away with only using your keyboard. However, I'm not aware of anything providing a similar experience for puzzles where you have to draw or mark edges, particularly if you may be too lazy to get out your mouse and would prefer to stick to using your keyboard like I do, which is why I wrote these programs.

After a while, though, I realized that to create something that I had a shot at getting other people to use, I pretty much had to make it web-based. [PUZ-PRE](http://pzv.jp/) is a big inspiration and player in this space; it offers a great UI, amazing list of puzzle types, and convenient URLs for sharing standard puzzle types, but I still wanted to approach the problem from a different angle, by primarily supporting the ability to freely and conveniently mark things on a grid, and only secondarily offering answer verification or more precise marks for any particular puzzle genre. Making it shareable would probably have required a redesign anyway because `gridderface` is pretty cavalier with regards to serializability. As it happens, I also wanted to try using scala.js, so here we are.

Name
----

As mentioned above, `edgeworth` aims to make drawing and marking edges on grids as easy as doing the same to cells. I've also become a big Ace Attorney fan since developing `gridderface`, and because [Miles Edgeworth's Magic Lawyer Superpower is basic logic](http://www.awkwardzombie.com/index.php?page=0&comic=030110), this name naturally suggested itself.
