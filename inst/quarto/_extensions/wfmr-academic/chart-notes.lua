local function has_class(el, class)
  for _, value in ipairs(el.classes) do
    if value == class then
      return true
    end
  end
  return false
end

local function present(value)
  return value ~= nil and value ~= ""
end

local function parse_blocks(markdown)
  return pandoc.read(markdown, "markdown+citations+tex_math_dollars").blocks
end

local function append_inlines(target, inlines)
  for _, inline in ipairs(inlines) do
    table.insert(target, inline)
  end
end

local function markdown_inlines(markdown)
  local blocks = parse_blocks(markdown)
  local inlines = {}

  for _, block in ipairs(blocks) do
    if #inlines > 0 then
      table.insert(inlines, pandoc.Space())
    end

    if block.t == "Para" or block.t == "Plain" then
      append_inlines(inlines, block.content)
    else
      table.insert(inlines, pandoc.Str(pandoc.utils.stringify(block)))
    end
  end

  return inlines
end

local function sentence(markdown)
  if markdown:match("[%.%!%?]$") then
    return markdown
  end

  return markdown .. "."
end

local function add_space_if_needed(inlines)
  if #inlines > 0 then
    table.insert(inlines, pandoc.Space())
  end
end

local function detail_inlines(markdown)
  return markdown_inlines(sentence(markdown))
end

local function notes_div(inlines, latex)
  local note_inlines = {}
  append_inlines(note_inlines, inlines)

  if latex then
    table.insert(
      note_inlines,
      1,
      pandoc.RawInline(
        "latex",
        "\\par\\nopagebreak[4]\\vspace{0.35em}\\begingroup\\raggedright\\footnotesize\\linespread{0.92}\\selectfont{}"
      )
    )
    table.insert(note_inlines, pandoc.RawInline("latex", "\\par\\endgroup"))
  end

  return pandoc.Div(
    { pandoc.Para(note_inlines) },
    pandoc.Attr("", { "chart-notes", "figure-notes" }, {})
  )
end

local function raw_block_contains(block, text)
  return block.t == "RawBlock" and block.text:find(text, 1, true) ~= nil
end

local function insert_latex_notes_before_float_end(blocks, note_block)
  for i, child in ipairs(blocks) do
    if raw_block_contains(child, "\\end{figure}") or raw_block_contains(child, "\\end{table}") then
      table.insert(blocks, i, note_block)
      return true
    end

    if child.content and insert_latex_notes_before_float_end(child.content, note_block) then
      return true
    end
  end

  return false
end

local function insert_latex_notes_inside_float(el, note_block)
  for _, block in ipairs(el.content) do
    if block.t == "Div" and has_class(block, "cell-output-display") then
      return insert_latex_notes_before_float_end(block.content, note_block)
    end
  end

  return false
end

local function append_latex_notes_to_output(el, note_block)
  for _, block in ipairs(el.content) do
    if block.t == "Div" and has_class(block, "cell-output-display") then
      table.insert(block.content, note_block)
      return true
    end
  end

  return false
end

local function clear_chart_attributes(el)
  el.attributes["chart-title"] = nil
  el.attributes["chart-subtitle"] = nil
  el.attributes["chart-notes"] = nil
  el.attributes["chart-source"] = nil
end

function Div(el)
  if not has_class(el, "cell") then
    return nil
  end

  local notes = el.attributes["chart-notes"]
  local source = el.attributes["chart-source"]
  local subtitle = el.attributes["chart-subtitle"]
  local has_chart_title = present(el.attributes["chart-title"])
  local has_chart_subtitle = present(el.attributes["chart-subtitle"])

  if not present(subtitle) and not present(notes) and not present(source) then
    if has_chart_title or has_chart_subtitle then
      clear_chart_attributes(el)
      return el
    end
    return nil
  end

  local inlines = {}
  if present(subtitle) then
    append_inlines(inlines, detail_inlines(subtitle))
  end
  if present(notes) then
    add_space_if_needed(inlines)
    append_inlines(inlines, detail_inlines(notes))
  end
  if present(source) then
    add_space_if_needed(inlines)
    append_inlines(inlines, detail_inlines(source))
  end

  if FORMAT:match("latex") then
    local note_block = notes_div(inlines, true)

    if insert_latex_notes_inside_float(el, note_block) then
      clear_chart_attributes(el)
      return el
    end

    if append_latex_notes_to_output(el, note_block) then
      clear_chart_attributes(el)
      return el
    end

    table.insert(el.content, note_block)
    clear_chart_attributes(el)
    return el
  end

  table.insert(el.content, notes_div(inlines, false))
  clear_chart_attributes(el)

  return el
end
