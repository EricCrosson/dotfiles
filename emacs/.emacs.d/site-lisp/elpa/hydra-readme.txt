This package can be used to tie related commands into a family of
short bindings with a common prefix - a Hydra.

Once you summon the Hydra (through the prefixed binding), all the
heads can be called in succession with only a short extension.
The Hydra is vanquished once Hercules, any binding that isn't the
Hydra's head, arrives.  Note that Hercules, besides vanquishing the
Hydra, will still serve his orignal purpose, calling his proper
command.  This makes the Hydra very seamless, it's like a minor
mode that disables itself automagically.

Here's how to use the examples bundled with Hydra:

   (require 'hydra-examples)
   (hydra-create "C-M-y" hydra-example-move-window-splitter)
   (hydra-create "M-g" hydra-example-goto-error)

You can expand the examples in-place, it still looks elegant:

    (hydra-create "<f2>"
      '(("g" text-scale-increase "zoom in")
        ("l" text-scale-decrease "zoom out")))

The third element of each list is the optional doc string that will
be displayed in the echo area when `hydra-is-helpful' is t.
