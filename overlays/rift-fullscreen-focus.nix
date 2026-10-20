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
# - keep every member of a stacked container visible across focus changes:
#   a click or focus raise moves its window above the cascade while the rest
#   keep their relative order, and a member covered from both sides of its
#   slot (e.g. clicking the left slot while the middle slot's window is
#   frontmost) was buried completely. The engine reports the focused
#   window's stacked container on selection changes and on entering the
#   stack mode; the reactor reads the members' true window-server z-order
#   and, when a member is covered from both sides (or the focused window
#   still has to be raised, which scrambles the cascade itself), raises the
#   buried members one raise-manager sequence at a time in bottom-up order
#   and the focused window last. Valid cascades are left untouched: a
#   neighbor click costs no raises at all; a jump over a member costs one
#   corrective raise plus the focus raise. Raise-induced focus events
#   resolve to transient key windows, so a restack planned while raises are
#   in flight (550ms window, covering the raise manager's 250ms raise and
#   250ms focus phases) is not discarded but re-armed: the next layout
#   response after the storm closes corrects the cascade without another
#   focus change. A focus raise whose simulated result keeps every edge
#   visible needs no correctives at all.
# - a raise-manager timeout cancels a hung corrective raise and delivers
#   only the focus, which left the cascade buried until the next
#   interaction (measured: ~8s with Slack); the reactor now tracks the
#   restack's members and retries the plan on a raise timeout, bounded at
#   three attempts within 2s. Residual: cross-app raise landings can
#   interleave for one to two compositor frames, so a buried intermediate
#   can appear below the 2ms sampling resolution of the probe.
# Measured live with a window-server probe (432 points, ~4ms sampling):
# two-window round trips showed no third window or desktop at any point.
# With four windows, 34 of 38 transitions showed none either; the other 4
# caught one window's move landing before its resize (AX has no atomic
# frame write): a strip of up to 72px at the right edge for 5-22ms, once a
# ~530px region for 43ms. About half also caught a window mid-move in a
# strip ~36px wide.
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
