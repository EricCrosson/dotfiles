--       ██████╗ █████╗ ██╗     ███████╗███╗   ██╗██████╗  █████╗ ██████╗
--      ██╔════╝██╔══██╗██║     ██╔════╝████╗  ██║██╔══██╗██╔══██╗██╔══██╗
--      ██║     ███████║██║     █████╗  ██╔██╗ ██║██║  ██║███████║██████╔╝
--      ██║     ██╔══██║██║     ██╔══╝  ██║╚██╗██║██║  ██║██╔══██║██╔══██╗
--      ╚██████╗██║  ██║███████╗███████╗██║ ╚████║██████╔╝██║  ██║██║  ██║
--       ╚═════╝╚═╝  ╚═╝╚══════╝╚══════╝╚═╝  ╚═══╝╚═════╝ ╚═╝  ╚═╝╚═╝  ╚═╝

-- ===================================================================
-- Initialization
-- ===================================================================

local awful = require("awful")
local gears = require("gears")
local wibox = require("wibox")
local beautiful = require("beautiful")
local dpi = beautiful.xresources.apply_dpi

local calendar = {}

-- ===================================================================
-- Create Widget
-- ===================================================================

calendar.create = function(screen)
    -- Clock / Calendar 12h format
    -- Get Time/Date format using `man strftime`
    local clock_widget = wibox.widget.textclock("<span font='" .. beautiful.font .. "'>%b %d %l:%M %p</span>", 1)

    local cal_shape = function(cr, width, height)
        gears.shape.partially_rounded_rect(cr, width, height, false, false, true, true, 12)
    end

    -- Calendar Widget
    local month_calendar = awful.widget.calendar_popup.month({
        screen = screen,
        start_sunday = true,
        spacing = 10,
        font = beautiful.font,
        long_weekdays = true,
        margin = 0, -- 10
        style_month = { border_width = 0, shape = cal_shape, padding = 25 },
        style_header = { border_width = 0, bg_color = "#00000000" },
        style_weekday = { border_width = 0, bg_color = "#00000000" },
        style_normal = { border_width = 0, bg_color = "#00000000" },
        style_focus = { border_width = 0, bg_color = "#8AB4F8" },
    })

    -- Attach calentar to clock_widget
    month_calendar:attach(clock_widget, "tc", { on_pressed = true, on_hover = false })

    return clock_widget
end

return calendar
