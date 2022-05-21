local settings = require "settings"

-- search engines

local engines = settings.window.search_engines
engines.aur    = "https://aur.archlinux.org/packages.php?O=0&K=%s&do_Search=Go"
engines.aw     = "https://wiki.archlinux.org/index.php/Special:Search?fulltext=Search&search=%s"
engines.google = "https://www.google.com/search?name=f&hl=en&q=%s"

engines.default = engines.google

-- zoom levels

settings.on["news.ycombinator.com"].webview.zoom_level = 125

local downloads = require "downloads"
downloads.default_dir = os.getenv("HOME") .. "/Downloads"
