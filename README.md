# FuncProg5
Repository for the fifth and final assignment of the Functional Programming course. Task pertains to the creation of decryption and encryption functions for a rail fence cipher in Clojure. 

The program still has bugs, and it seems that the bug is actually pretty deep in and is very problematic to fix, as every attempt I've made to fix the bug has made the program completely useless, as in it goes from failing a few tests to failing all of them. The bug results in the program sometimes not being able to decrypt a message. I think this happens whenever the cycle is larger than the amount of rails and the cycle is on its way back to the first row with the last letter being in one of the middle rows. This results in the program becoming progressively worse as the amount of rails is increased. The result, as far as I know, is always CORRECT, the problem is the fact that the program sometimes can't execute in the first place. I'll try to fix this whenever I have time but I've already invested an hour or two into fixing it and still haven't figured out how to fix this.

For all intents and purposes this assignment can be considered to be in its final form.
