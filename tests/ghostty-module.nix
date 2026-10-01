{pkgs}: let
  inherit (pkgs) lib;
  helpers = import ./helpers.nix {inherit lib;};
  inherit (helpers) assertEq;

  module = import ../profiles/eric/modules/ghostty.nix;

  # ghostty.nix branches on pkgs.stdenv.hostPlatform.isDarwin; evaluate the
  # module for both platforms. Eval-only: nothing here is built, and the
  # linux-only `pkgs.ghostty` package reference is never forced.
  mkPkgs = isDarwin:
    pkgs
    // {
      stdenv =
        pkgs.stdenv
        // {
          hostPlatform =
            pkgs.stdenv.hostPlatform
            // {inherit isDarwin;};
        };
    };

  eval = isDarwin:
    (lib.evalModules {
      specialArgs = {pkgs = mkPkgs isDarwin;};
      modules = [
        module
        {
          # Stub out the home-manager surface ghostty.nix touches.
          options = {
            appearance-sync.enable = lib.mkOption {
              type = lib.types.bool;
              default = false;
            };
            home.file = lib.mkOption {
              type = lib.types.attrsOf lib.types.anything;
              default = {};
            };
            programs.ghostty = lib.mkOption {
              type = lib.types.attrsOf lib.types.anything;
            };
          };
        }
      ];
    })
    .config;

  keybinds = isDarwin: (eval isDarwin).programs.ghostty.settings.keybind;
  has = list: binding:
    builtins.elem binding list;

  countInfix = needle: list:
    lib.length (lib.filter (lib.hasInfix needle) list);

  darwin = keybinds true;
  linux = keybinds false;

  # macOS: cmd+d leader (Ghostty trigger "super"), including the unbind of
  # the default single-press cmd+d split-right that would shadow the sequence.
  test-darwin-leader = assert assertEq "darwin split-right leader" (has darwin "super+d>v=new_split:right") true;
  assert assertEq "darwin split-down leader" (has darwin "super+d>s=new_split:down") true;
  assert assertEq "darwin unbinds default cmd+d" (has darwin "super+d=unbind") true;
  assert assertEq "darwin has no alt leader" (countInfix "alt+d>" darwin) 0; true;

  # Linux: left-alt leader (Ghostty trigger "alt"; no left/right Alt
  # distinction). Ghostty's Linux defaults bind neither alt+d nor super+d,
  # so no unbind is needed and no super leader may leak in.
  test-linux-leader = assert assertEq "linux split-right leader" (has linux "alt+d>v=new_split:right") true;
  assert assertEq "linux split-down leader" (has linux "alt+d>s=new_split:down") true;
  assert assertEq "linux has no super leader" (countInfix "super+d>" linux) 0; true;

  # Every non-leader keybind transfers unchanged to both platforms.
  test-shared-keybinds = let
    shared = [
      "ctrl+shift+h=goto_split:left"
      "ctrl+shift+j=goto_split:down"
      "ctrl+shift+k=goto_split:up"
      "ctrl+shift+l=goto_split:right"
      "ctrl+shift+z=toggle_split_zoom"
      "ctrl+shift+w=close_surface"
      "ctrl+shift+equal=equalize_splits"
    ];
  in
    assert assertEq "shared keybinds transfer to linux" (lib.all (has linux) shared) true;
    assert assertEq "shared keybinds transfer to darwin" (lib.all (has darwin) shared) true; true;
in
  assert test-darwin-leader;
  assert test-linux-leader;
  assert test-shared-keybinds; "all tests passed"
