local section_colours = {
  ["section-salmon"] = "#D7827E",
  ["section-teal"] = "#56949F",
  ["section-purple"] = "#907AA9",
  ["section-lime"] = "#8B9A5A",
  ["section-yellow"] = "#EA9D34",
  ["section-blue"] = "#286983",
  ["section-pink"] = "#B4637A",
  ["wfmr-section-salmon"] = "#D7827E",
  ["wfmr-section-teal"] = "#56949F",
  ["wfmr-section-purple"] = "#907AA9",
  ["wfmr-section-lime"] = "#8B9A5A",
  ["wfmr-section-yellow"] = "#EA9D34",
  ["wfmr-section-blue"] = "#286983",
  ["wfmr-section-pink"] = "#B4637A"
}

local function has_class(el, class)
  for _, value in ipairs(el.classes) do
    if value == class then
      return true
    end
  end
  return false
end

local function add_class(el, class)
  if not has_class(el, class) then
    table.insert(el.classes, class)
  end
end

local function present(value)
  return value ~= nil and value ~= ""
end

function Header(el)
  if not FORMAT:match("revealjs") then
    return nil
  end

  for class, colour in pairs(section_colours) do
    if has_class(el, class) then
      add_class(el, "wfmr-section-header")

      if not present(el.attributes["data-background-color"]) then
        el.attributes["data-background-color"] = colour
      end

      return el
    end
  end

  return nil
end
