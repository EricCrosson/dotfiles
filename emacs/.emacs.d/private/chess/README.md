# Chess contribution layer for Spacemacs

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc/generate-toc again -->
**Table of Contents**

- [Chess contribution layer for Spacemacs](#chess-contribution-layer-for-spacemacs)
    - [Description](#description)
    - [Install](#install)
    - [Key bindings](#key-bindings)

<!-- markdown-toc end -->

## Description

This layer provides emacs-chess and spacemacs bindings for commonly used
functions.

## Install

To use this contribution add it to your `~/.spacemacs`

```elisp
(setq-default dotspacemacs-configuration-layers '(chess))
```

## Key bindings

Key Binding     | Description
----------------|------------------------------------------------------------
`<SPC> a g c c` | chess
`<SPC> a g c i` | chess-ics
`<SPC> a g c p` | chess-puzzle
`<SPC> a g c l` | chess-pgn-read
`<SPC> a g c t` | chess-display-chat
`<SPC> a g c s` | chess-display-pass
`<SPC> a g c b` | chess-display-abort
`<SPC> a g c m` | chess-display-match
`<SPC> a g c f` | chess-display-force
`<SPC> a g c a` | chess-display-accept
`<SPC> a g c r` | chess-display-resign
`<SPC> a g c e` | chess-display-create
