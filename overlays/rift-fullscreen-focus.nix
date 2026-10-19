# This overlay patches rift-wm so fullscreen toggles act on the active window.
#
# Context: rift's fullscreen toggles acted on the layout tree's selection,
# which can trail the actually focused window when a focus notification was
# missed. The toggle's response raises (and thereby activates) its target, so
# fullscreening buried the window the user was typing in, activated the wrong
# one, and the toggle/untoggle round trip flipped the window stacking order.
#
# Fix: pin the tree selection to the engine's focused window before both
# fullscreen toggles. Backported from EricCrosson/rift commits f034caf
# ("fix(layout): toggle fullscreen on the active window, not a stale
# selection") and 9f7d64d ("fix(layout): toggle fullscreen-within-gaps on the
# active window too") against rift 0.6.7. The regression test
# fullscreen_toggle_targets_focused_window_not_stale_selection is included
# and passes against the 0.6.7 tree; it also runs in rift's own CI on the
# main branch.
#
# doCheck is disabled: rift's macOS-Space-sensitive reactor tests fail in a
# local sandbox on this host regardless of the patch (unpatched
# nixpkgs fe132422 rift-wm 0.6.7 fails 9 tests here, e.g.
# stale_user_space_disappearance_does_not_restore_old_display_assignment,
# and one run SIGABRTs in an ObjC weak-reference teardown). nixpkgs already
# skips one such flaky test in checkFlags. Drop this overlay once the fix
# lands in a nixpkgs rift-wm release.
_final: prev: {
  rift-wm = prev.rift-wm.overrideAttrs (old: {
    patches = (old.patches or []) ++ [./rift-fullscreen-focus.patch];
    doCheck = false;
  });
}
