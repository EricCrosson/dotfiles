# This overlay patches rift-wm so fullscreen toggles and the stack layout
# (used as a whole-workspace fullscreen view by Alt+Comma in
# profiles/eric/modules/rift.nix) switch instantly with the selected window
# in front, and a round trip restores both the arrangement and the selected
# window.
#
# Context (rift 0.6.7):
# - Toggles and layout switches acted on the layout tree's selection, which
#   can trail the focused window when a focus notification was missed.
# - set-layout raised every window of the workspace. Each raise activates its
#   app; the activations raced, and whichever landed last stole focus and the
#   selection (a stack round trip started on Ghostty and came back on Slack)
#   and was drawn over the selected window mid-switch.
# - Every layout-mode switch rebuilt the tree from scratch, so a stack round
#   trip lost the traditional tree's shape and sizes (346/1382 -> 864/864).
# - Instant arranges wrote every window concurrently, so whichever app was
#   fastest landed first: background windows were caught mid-move in front
#   of the old tiles, and a shrinking stack front revealed unmoved neighbors.
#
# Fix (branch backport/0.6.7 of EricCrosson/rift, on the v0.6.7 tag):
# - carry the focused window as the selection across toggles and layout
#   switches, and raise only that window;
# - keep the arrangement a workspace leaves when it enters the stack layout
#   and restore it, reconciling windows that came and went, when leaving the
#   stack for the mode it came from; every other switch still rebuilds, which
#   the Alt+Shift+R jail-break relies on;
# - preserve sibling order when a switch does rebuild;
# - apply fullscreen toggles instantly in both directions, and write window
#   position before size (macOS clamps size writes at the screen edge);
# - stage the instant writes of toggles and layout switches by z-order: a
#   growing foreground first, a shrinking one last, the second stage once the
#   first shows in the window server (a clamped size counts; 150ms deadline),
#   dropping held writes that a newer arrange superseded;
# - give stack slots only to windows that can cover them, so the stack always
#   covers the screen (a non-resizable 1x32 Safari window left a 40px strip
#   at the right edge showing the desktop).
# Measured live with a window-server probe, two-window round trips show no
# third window or desktop at any probed point. With three or more windows a
# 40px peek strip can still flicker (desktop or a neighbor, up to ~70ms)
# where one window's position and size land separately (AX has no atomic
# frame write).
# Regression tests in the patch cover the selection, restore, order, staging
# and stack coverage fixes (each observed failing before its fix) and the
# instant toggles; all pass against the 0.6.7 tree. The position-before-size
# order was verified live; its unit test needs main's FrameTarget mock. None
# has run in upstream CI yet.
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
