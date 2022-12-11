local vertical_tile = {}

vertical_tile.name = "V"

function vertical_tile.arrange(p)
    local area
    area = p.workarea
    local i = 0
    for _, c in pairs(p.clients) do
        local g = {
            x = area.x,
            y = area.y + i * (area.height / #p.clients),
            width = area.width,
            height = area.height / #p.clients,
        }
        p.geometries[c] = g
        i = i + 1
    end
end

return vertical_tile
