--      ████████╗ ██████╗ ██████╗     ██████╗  █████╗ ███╗   ██╗███████╗██╗
--      ╚══██╔══╝██╔═══██╗██╔══██╗    ██╔══██╗██╔══██╗████╗  ██║██╔════╝██║
--         ██║   ██║   ██║██████╔╝    ██████╔╝███████║██╔██╗ ██║█████╗  ██║
--         ██║   ██║   ██║██╔═══╝     ██╔═══╝ ██╔══██║██║╚██╗██║██╔══╝  ██║
--         ██║   ╚██████╔╝██║         ██║     ██║  ██║██║ ╚████║███████╗███████╗
--         ╚═╝    ╚═════╝ ╚═╝         ╚═╝     ╚═╝  ╚═╝╚═╝  ╚═══╝╚══════╝╚══════╝

-- ===================================================================
-- Initialization
-- ===================================================================

local awful = require("awful")
local beautiful = require("beautiful")
local gears = require("gears")
local wibox = require("wibox")
local dpi = beautiful.xresources.apply_dpi

-- import widgets
local task_list = require("widgets.task-list")

-- define module table
local top_panel = {}

-- ===================================================================
-- Bar Creation
-- ===================================================================

-- Create a wibox for each screen and add it
local taglist_buttons = gears.table.join(
    awful.button({}, 1, function(t)
        t:view_only()
    end),
    awful.button({ modkey }, 1, function(t)
        if client.focus then
            client.focus:move_to_tag(t)
        end
    end),
    awful.button({}, 3, awful.tag.viewtoggle),
    awful.button({ modkey }, 3, function(t)
        if client.focus then
            client.focus:toggle_tag(t)
        end
    end),
    awful.button({}, 4, function(t)
        awful.tag.viewnext(t.screen)
    end),
    awful.button({}, 5, function(t)
        awful.tag.viewprev(t.screen)
    end)
)

top_panel.create = function(s)
    local panel = awful.wibar({
        screen = s,
        position = "top",
        ontop = true,
        height = beautiful.top_panel_height,
        width = s.geometry.width,
    })

    s.tag_list = awful.widget.taglist({
        screen = s,
        filter = awful.widget.taglist.filter.all,
        buttons = taglist_buttons,
    })

    panel:setup({
        expand = "none",
        layout = wibox.layout.align.horizontal,
        {
            layout = wibox.layout.fixed.horizontal,
            s.tag_list,
            task_list.create(s),
        },
        require("widgets.calendar").create(s),
        {
            layout = wibox.layout.fixed.horizontal,
            wibox.layout.margin(wibox.widget.systray(), dpi(5), dpi(5), dpi(5), dpi(5)),
            -- require("widgets.bluetooth"),
            -- require("widgets.network")(),
            -- require("widgets.battery"),
            wibox.layout.margin(require("widgets.layout-box"), dpi(5), dpi(5), dpi(5), dpi(5)),
        },
    })

    -- ===================================================================
    -- Functionality
    -- ===================================================================

    -- hide panel when client is fullscreen
    local function change_panel_visibility(client)
        if client.screen == s then
            panel.ontop = not client.fullscreen
        end
    end

    -- connect panel visibility function to relevant signals
    client.connect_signal("property::fullscreen", change_panel_visibility)
    client.connect_signal("focus", change_panel_visibility)
end

return top_panel
