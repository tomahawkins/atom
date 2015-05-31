Atom: An EDSL for Embedded Hard Realtime Applications
====

Atom is a Haskell EDSL for designing hard realtime embedded software. Based on guarded atomic actions (similar to STM), Atom enables highly concurrent programming without the need for mutex locking. In addition, Atom performs compile-time task scheduling and generates code with deterministic execution time and constant memory use, simplifying the process of timing verification and memory consumption in hard realtime applications. Without mutex locking and run-time task scheduling, Atom eliminates the need and overhead of RTOSes for many embedded applications.

# Additional Information
 - [Hackage package](http://hackage.haskell.org/package/atom).
 - Homepage of [Tom Hawkins](http://tomahawkins.org/), the original creator.
 - [CUFP 2008](http://cufp.galois.com/2008/schedule.html) talk, "Controlling Hybrid Vehicles with Haskell"
 - [Monitoring Distributed Real-Time Systems](http://www.cs.indiana.edu/~lepike/pubs/survey.pdf) from Alwyn Goodloe and Lee Pike
