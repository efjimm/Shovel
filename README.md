# Shovel

Shovel is a TUI library written in Zig, exposing a robust and simple interface
for creating TUI programs. Shovel started as a fork of
[zig-spoon](https://git.sr.ht/~leon_plickat/zig-spoon) and has since
significantly outgrown the original code.

Why fork zig-spoon? Because I was using it for a personal project and kept adding features I wanted.
By the time I thought about publishing this repo the changes were extensive enough that I was too
lazy to contribute them upstream.

The design of Shovel is inherently decoupled, so it is not necessary to use
everything in the library. For example, Shovel implements a terminfo parser
which is used internally but can also be used standalone.

Shovel supports the kitty keyboard protocol.

This readme could be a lot better, huh?.

**The following is taken from the README of zig-spoon, the project that Shovel
was originally forked from. This all still applies so I left it in.**

I am happy you are interested in this project and I hope it will serve you well.
However before you continue, I'd like to quickly explain and mention a few
things, that will make working with zig-spoon clearer. You can consider this to
be part of the documentation, if you like.

Despite being a modern library written in a modern language, zig-spoon will
always be held back by the countless legacy interfaces and hacks it tries to
abstract away from you. This is inevitable. While you shouldn't need to worry
about this too much, there are a few things you probably want to know.

Unless the terminal your zig-spoon application is running in supports the kitty
keyboard protocol, keyboard input has certain limitations. It is impossible to
differentiate between `M-a` or the user simply pressing `escape` and `a` in
sequence fast enough (`a` is an example, this holds for all keys). It is
impossible to differentiate `C-m`, `C-j` and `enter`, so to err on the side of
caution, zig-spoon will always assume `enter` is the intended key press.
Any non-ascii characters may behave in an unexpected manner, depending on the
terminal emulator. The super modifier is only supported when the kitty keyboard
protocol is used.

If the kitty keyboard protocol is available, zig-spoon will use its lowest mode.
This way zig-spoon can use the same input system for both legacy and kitty
inputs, which is nicer for both library and application developers. Additional
information that can be added to the input system without compromising its
support for legacy inputs will be implemented where possible / feasible / useful.
While the higher kitty keyboard modes offer even more information about inputs,
they are generally not needed for the average terminal application. The lowest
mode already fixes all major input annoyances that were outlined in the previous
paragraph. If you do need the extra information, you can easily replace
zig-spoons input parser with your own custom one.

The traditional 16 terminal colours may differ between background versus
foreground and normal versus bold text. So `.{ .fg = .red }` and
`.{ .bg = .red, .reverse = true }` may look different, despite describing the
same attributes in theory. Also the colours from the 256-colours mode can differ
drastically between terminal emulators. If you value acuarate colours, you
probably should use the RGB-colour mode.

And certainly the saddest caveat: Not all terminal emulators support blinking
text.

If you face any issues or unexpected oddities, please create an issue detailing
them.
