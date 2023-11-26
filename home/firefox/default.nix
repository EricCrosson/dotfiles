{
  pkgs,
  inputs,
  ...
}:
# TODO: set font to Hack
# FIXME: screen tearing
{
  programs = {
    firefox = {
      enable = true;
      package =
        # x86_64-linux is the only supported system!
        # https://github.com/colemickens/flake-firefox-nightly/blob/101d73a7586b88269949ed5fe332bfba7ce12af6/flake.nix#L23
        if pkgs.system == "x86_64-linux"
        then inputs.firefox.packages.${pkgs.system}.firefox-nightly-bin
        # FIXME: firefox isn't supported on aarch64-linux either
        else pkgs.firefox-bin;
      profiles = {
        default = {
          id = 0;
          name = "Default";
          isDefault = true;
          bookmarks = [
            {
              name = "Discord";
              keyword = "discord";
              url = "https://discord.com/channels/@me";
            }
            {
              name = "GitHub Notifications";
              keyword = "n";
              url = "https://github.com/notifications?query=is%3Aunread";
            }
            {
              name = "GitHub Pull Requests";
              keyword = "p";
              url = "https://github.com/pulls";
            }
            {
              name = "Helix Editor Keymap";
              keyword = "hx";
              url = "https://docs.helix-editor.com/master/keymap.html";
            }
          ];
          extensions = with pkgs.nur.repos.rycee.firefox-addons; [
            onepassword-password-manager
            # To disable cursor animation on Google Docs:
            # `.docs-text-ui-cursor-blink { animation: none !important; }`
            stylus
            ublock-origin

            # For Rapid Serial Visual Presentation, reedy is nice (must be installed manually for now)
            # https://github.com/skywinder/Reedy-for-Firefox
          ];
          search = {
            default = "Google";
            engines = {
              "Nix Packages" = {
                urls = [
                  {
                    template = "https://search.nixos.org/packages";
                    params = [
                      {
                        name = "type";
                        value = "packages";
                      }
                      {
                        name = "query";
                        value = "{searchTerms}";
                      }
                    ];
                  }
                ];
                icon = "${pkgs.nixos-icons}/share/icons/hicolor/scalable/apps/nix-snowflake.svg";
                definedAliases = ["np"];
              };
              "Amazon.com".metaData.hidden = true;
              "Bing".metaData.hidden = true;
              "DuckDuckGo".metaData.hidden = true;
              "eBay".metaData.hidden = true;
              "Wikipedia (en)".metaData.alias = "w";
            };
            force = true;
          };
          settings = {
            "app.normandy.enabled" = false;
            "browser.contentblocking.category" = "strict";
            "browser.startup.page" = 3; # Restore previous windows and tabs on startup.
            "extensions.htmlaboutaddons.inline-options.enabled" = false;
            "extensions.htmlaboutaddons.recommendations.enabled" = false;
            "extensions.pocket.enabled" = false;
            "extensions.pocket.showHome" = false;
            "privacy.donottrackheader.enabled" = true;
            "browser.newtabpage.activity-stream.asrouter.userprefs.cfr.addons" = false;
            "browser.newtabpage.activity-stream.asrouter.userprefs.cfr.features" = false;
            "browser.newtabpage.activity-stream.feeds.section.highlights" = false;
            "browser.newtabpage.activity-stream.feeds.section.topstories" = false;
            "browser.newtabpage.activity-stream.feeds.snippets" = false;
            "browser.newtabpage.activity-stream.feeds.topsites" = false;
            "browser.newtabpage.activity-stream.section.highlights.includeBookmarks" = false;
            "browser.newtabpage.activity-stream.section.highlights.includeDownloads" = false;
            "browser.newtabpage.activity-stream.section.highlights.includePocket" = false;
            "browser.newtabpage.activity-stream.section.highlights.includeVisited" = false;
            "browser.newtabpage.activity-stream.section.highlights.rows" = false;
            "browser.newtabpage.activity-stream.section.topstories.rows" = false;
            "browser.newtabpage.activity-stream.showSponsored" = false;
            "browser.newtabpage.activity-stream.topSitesRows" = false;
            "browser.newtabpage.activity-stream.showSponsoredTopSites" = false;
            "browser.urlbar.suggest.quicksuggest.sponsored" = false;
            "media.eme.enabled" = true;

            # Opt out of all telemetry
            "browser.newtabpage.activity-stream.feeds.telemetry" = false;
            "browser.newtabpage.activity-stream.telemetry" = false;
            "browser.ping-centre.telemetry" = false;
            "datareporting.healthreport.uploadEnabled" = false;
            "datareporting.policy.dataSubmissionEnabled" = false;
            "datareporting.sessions.current.clean" = true;
            "devtools.onboarding.telemetry.logged" = false;
            "toolkit.telemetry.archive.enabled" = false;
            "toolkit.telemetry.bhrPing.enabled" = false;
            "toolkit.telemetry.enabled" = false;
            "toolkit.telemetry.firstShutdownPing.enabled" = false;
            "toolkit.telemetry.hybridContent.enabled" = false;
            "toolkit.telemetry.newProfilePing.enabled" = false;
            "toolkit.telemetry.reportingpolicy.firstRun" = false;
            "toolkit.telemetry.shutdownPingSender.enabled" = false;
            "toolkit.telemetry.unified" = false;
            "toolkit.telemetry.updatePing.enabled" = false;
          };
        };
      };
    };
  };
}
