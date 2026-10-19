# This overlay patches rift-wm so fullscreen toggles act on the active window,
# and without animation.
#
# Context: rift's fullscreen toggles acted on the layout tree's selection,
# which can trail the actually focused window when a focus notification was
# missed. The toggle's response raises (and thereby activates) its target, so
# fullscreening buried the window the user was typing in, activated the wrong
# one, and the toggle/untoggle round trip flipped the window stacking order.
# Separately, the fullscreen toggle was animated: the interpolated frames
# sweep the window over its siblings and expose every window behind it,
# reading as a distracting flash.
#
# Fix: pin the tree selection to the engine's focused window before both
# fullscreen toggles, rebuild the tree in tree order (not selection order)
# across layout-mode switches, apply the fullscreen toggle's arrange pass
# instantly (no animation), and write window position before size when
# shrinking so the intermediate frame overhangs off-screen instead of
# exposing the region behind the old wide frame (the fullscreen-exit flash;
# macOS clamps size writes to the screen edge, so the order matters).
# Backported from EricCrosson/rift commits f034caf ("fix(layout): toggle
# fullscreen on the active window, not a stale selection"), 9f7d64d
# ("fix(layout): toggle fullscreen-within-gaps on the active window too"),
# 4fe24e5 ("fix(layout): preserve sibling order across layout-mode
# switches"), e309144 ("fix(reactor): apply fullscreen toggles without
# animation") and 00c25b4 ("fix(app): write window position before size when
# shrinking") against rift 0.6.7. The regression tests
# fullscreen_toggle_targets_focused_window_not_stale_selection,
# layout_mode_switch_preserves_window_order and
# fullscreen_toggles_arrange_instantly_without_animation are included and
# were verified locally against the 0.6.7 tree (they fail without the source
# changes); the write-order fix is verified live (captured compositor
# frames) and carries its unit test on main only, where the FrameTarget
# mock exists. None of these have run in upstream CI yet - the fixes are
# local commits.
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
