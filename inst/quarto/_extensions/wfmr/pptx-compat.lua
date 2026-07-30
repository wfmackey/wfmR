-- PowerPoint translation layer for the wfmR Quarto extension.
--
-- Pandoc's PowerPoint writer does not interpret RevealJS CSS classes. This
-- filter maps the small, reusable wfmR vocabulary to native DrawingML while
-- keeping the source document and generated PowerPoint objects editable.

if not FORMAT:match("pptx") then
  return {}
end

local EMU_PER_PIXEL = 9525

local palette = {
  base = "FAF4ED",
  surface = "FFFAF3",
  overlay = "F2E9E1",
  muted = "9893A5",
  subtle = "797593",
  text = "575279",
  love = "B4637A",
  gold = "EA9D34",
  rose = "D7827E",
  pine = "286983",
  foam = "56949F",
  iris = "907AA9",
  lime = "8B9A5A",
  label_object = "E5EBEE",
  label_param = "F5DEDD",
  label_welfare = "E3EDEB",
  label_method = "EAE2EF",
  label_gold = "F7E9D3",
  label_love = "F2DFE6",
  label_moss = "E9EBDD"
}

local section_backgrounds = {
  ["section-salmon"] = "section-salmon.svg",
  ["section-teal"] = "section-teal.svg",
  ["section-purple"] = "section-purple.svg",
  ["section-lime"] = "section-lime.svg",
  ["section-yellow"] = "section-yellow.svg",
  ["section-blue"] = "section-blue.svg",
  ["section-pink"] = "section-pink.svg",
  ["wfmr-section-salmon"] = "section-salmon.svg",
  ["wfmr-section-teal"] = "section-teal.svg",
  ["wfmr-section-purple"] = "section-purple.svg",
  ["wfmr-section-lime"] = "section-lime.svg",
  ["wfmr-section-yellow"] = "section-yellow.svg",
  ["wfmr-section-blue"] = "section-blue.svg",
  ["wfmr-section-pink"] = "section-pink.svg"
}

local span_styles = {
  ["pub-meta"] = {color = "000000", size = 820},
  ["pub-data"] = {color = palette.rose, size = 820},
  ["pub-finding"] = {color = palette.iris, size = 820},
  ["label-teal"] = {color = palette.foam, highlight = palette.label_welfare, bold = true},
  ["label-salmon"] = {color = palette.rose, highlight = palette.label_param, bold = true},
  ["label-yellow"] = {color = palette.gold, highlight = palette.label_gold, bold = true},
  ["label-pink"] = {color = palette.love, highlight = palette.label_love, bold = true},
  ["label-lime"] = {color = palette.lime, highlight = palette.label_moss, bold = true},
  ["label-blue"] = {color = palette.pine, highlight = palette.label_object, bold = true},
  ["label-green"] = {color = palette.foam, highlight = palette.label_welfare, bold = true},
  ["label-purple"] = {color = palette.iris, highlight = palette.label_method, bold = true},
  ["def-object"] = {color = palette.pine, highlight = palette.label_object, bold = true},
  ["def-param"] = {color = palette.love, highlight = palette.label_param, bold = true},
  ["def-welfare"] = {color = palette.foam, highlight = palette.label_welfare, bold = true},
  ["def-method"] = {color = palette.iris, highlight = palette.label_method, bold = true},
  ["spike-label"] = {color = palette.gold, bold = true, size = 850}
}
-- The .gold/.foam/.rose/.pine/.iris/.love text colours are deliberately absent.
-- Mapping them here sends their spans through span_to_native, whose raw runs the
-- PowerPoint writer discards, so the words disappear. Leaving them unmapped
-- keeps the text and loses only the colour.

-- Colour macros used inside maths on the RevealJS deck. PowerPoint has no
-- MathJax, so the wrapper is dropped and only the maths inside is kept.
local math_colour_macros = {"gold", "foam", "rose", "pine", "iris", "love"}

local filter_dir = pandoc.path.directory(PANDOC_SCRIPT_FILE)
local backgrounds_dir = pandoc.path.join({filter_dir, "pptx-backgrounds"})

local function has_class(el, wanted)
  for _, class in ipairs(el.classes or {}) do
    if class == wanted then
      return true
    end
  end
  return false
end

local function first_class_style(el)
  for _, class in ipairs(el.classes or {}) do
    if span_styles[class] then
      return span_styles[class]
    end
  end
  return nil
end

local function merged_style(base, extra)
  local result = {}
  for key, value in pairs(base or {}) do
    result[key] = value
  end
  for key, value in pairs(extra or {}) do
    result[key] = value
  end
  return result
end

local function xml_escape(text)
  return tostring(text or "")
    :gsub("&", "&amp;")
    :gsub("<", "&lt;")
    :gsub(">", "&gt;")
    :gsub('"', "&quot;")
    :gsub("'", "&apos;")
end

local function pretty_math(text)
  local value = tostring(text or "")
  value = value:gsub("\\tau", "τ")
  value = value:gsub("\\delta", "δ")
  value = value:gsub("\\Pi", "Π")
  value = value:gsub("\\qquad", "  ")
  value = value:gsub("\\,", " ")
  value = value:gsub("\\ ", " ")
  value = value:gsub("[{}]", "")
  value = value:gsub("_([%w])", "%1")
  value = value:gsub("%^([%w])", "%1")
  return value
end

local function run_xml(text, style)
  local properties = { 'lang="en-AU"' }
  if style.size then table.insert(properties, 'sz="' .. tostring(style.size) .. '"') end
  if style.bold then table.insert(properties, 'b="1"') end
  if style.italic then table.insert(properties, 'i="1"') end
  if style.underline then table.insert(properties, 'u="sng"') end
  local children = {}
  if style.color then
    table.insert(children, '<a:solidFill><a:srgbClr val="' .. style.color .. '"/></a:solidFill>')
  end
  if style.highlight then
    table.insert(children, '<a:highlight><a:srgbClr val="' .. style.highlight .. '"/></a:highlight>')
  end
  if style.font then
    table.insert(children, '<a:latin typeface="' .. xml_escape(style.font) .. '"/>')
    table.insert(children, '<a:ea typeface="' .. xml_escape(style.font) .. '"/>')
    table.insert(children, '<a:cs typeface="' .. xml_escape(style.font) .. '"/>')
  end
  return '<a:r><a:rPr ' .. table.concat(properties, " ") .. '>' .. table.concat(children) .. '</a:rPr><a:t xml:space="preserve">' .. xml_escape(text) .. '</a:t></a:r>'
end

local function raw_run(text, style)
  return pandoc.RawInline("openxml", run_xml(text, style or {}))
end

local function render_inlines(inlines, base_style)
  local result = pandoc.Inlines({})

  local function append_inline(inline, style)
    if inline.t == "Str" then
      result:insert(raw_run(inline.text, style))
    elseif inline.t == "Space" or inline.t == "SoftBreak" then
      result:insert(raw_run(" ", style))
    elseif inline.t == "LineBreak" then
      result:insert(pandoc.RawInline("openxml", "<a:br/>"))
    elseif inline.t == "Emph" then
      local nested = render_inlines(inline.content, merged_style(style, {italic = true}))
      for _, item in ipairs(nested) do result:insert(item) end
    elseif inline.t == "Strong" then
      local nested = render_inlines(inline.content, merged_style(style, {bold = true}))
      for _, item in ipairs(nested) do result:insert(item) end
    elseif inline.t == "Code" then
      result:insert(raw_run(inline.text, merged_style(style, {font = "Menlo"})))
    elseif inline.t == "Math" then
      result:insert(raw_run(pretty_math(inline.text), style))
    elseif inline.t == "Link" then
      local nested = render_inlines(inline.content, merged_style(style, {color = palette.love, underline = true}))
      for _, item in ipairs(nested) do result:insert(item) end
    elseif inline.t == "Span" then
      local nested_style = merged_style(style, first_class_style(inline) or {})
      local nested = render_inlines(inline.content, nested_style)
      for _, item in ipairs(nested) do result:insert(item) end
    elseif inline.t == "RawInline" and inline.format == "openxml" then
      result:insert(inline)
    else
      local text = pandoc.utils.stringify(inline)
      if text ~= "" then result:insert(raw_run(text, style)) end
    end
  end

  for _, inline in ipairs(inlines or {}) do
    append_inline(inline, base_style or {})
  end
  return result
end

-- MathJax understands \gold{x} and \style{...}{x}; Pandoc's TeX reader does not,
-- and bails out to raw source. Removing the colour wrapper leaves ordinary TeX
-- that converts to real PowerPoint equations.
local function strip_math_colour(math)
  local cleaned = tostring(math.text or "")
  cleaned = cleaned:gsub("\\style%s*{[^}]*}", "")
  for _, name in ipairs(math_colour_macros) do
    cleaned = cleaned:gsub("\\" .. name .. "%f[^%a]", "")
  end
  if cleaned == math.text then return nil end
  return pandoc.Math(math.mathtype, cleaned)
end

local function span_to_native(span)
  local style = first_class_style(span)
  if not style then return nil end
  return render_inlines(span.content, style)
end

local function px(value)
  return math.floor(value * EMU_PER_PIXEL + 0.5)
end

local function rect_shape(id, name, x, y, width, height, fill)
  return '<p:sp><p:nvSpPr><p:cNvPr id="' .. id .. '" name="' .. xml_escape(name) .. '"/><p:cNvSpPr/><p:nvPr/></p:nvSpPr><p:spPr><a:xfrm><a:off x="' .. px(x) .. '" y="' .. px(y) .. '"/><a:ext cx="' .. px(width) .. '" cy="' .. px(height) .. '"/></a:xfrm><a:prstGeom prst="rect"><a:avLst/></a:prstGeom><a:solidFill><a:srgbClr val="' .. fill .. '"/></a:solidFill><a:ln><a:noFill/></a:ln></p:spPr></p:sp>'
end

local function text_shape(id, name, x, y, width, height, text, style)
  local body = run_xml(text, style or {})
  return '<p:sp><p:nvSpPr><p:cNvPr id="' .. id .. '" name="' .. xml_escape(name) .. '"/><p:cNvSpPr txBox="1"/><p:nvPr/></p:nvSpPr><p:spPr><a:xfrm><a:off x="' .. px(x) .. '" y="' .. px(y) .. '"/><a:ext cx="' .. px(width) .. '" cy="' .. px(height) .. '"/></a:xfrm><a:prstGeom prst="rect"><a:avLst/></a:prstGeom><a:noFill/><a:ln><a:noFill/></a:ln></p:spPr><p:txBody><a:bodyPr wrap="square" lIns="0" tIns="0" rIns="0" bIns="0" anchor="t"><a:noAutofit/></a:bodyPr><a:lstStyle/><a:p><a:pPr marL="0" indent="0"><a:lnSpc><a:spcPct val="108000"/></a:lnSpc><a:buNone/></a:pPr>' .. body .. '<a:endParaRPr/></a:p></p:txBody></p:sp>'
end

local function method_panel_shapes()
  local xml = {}
  local panels = {
    {410, "Method panel left 1", 158, 126, 462, 58, palette.label_method, palette.rose},
    {412, "Method panel left 2", 158, 284, 462, 58, palette.label_method, palette.rose},
    {414, "Method panel right 1", 660, 126, 462, 58, palette.label_method, palette.iris},
    {416, "Method panel right 2", 660, 338, 462, 65, palette.label_method, palette.rose}
  }
  for _, panel in ipairs(panels) do
    table.insert(xml, rect_shape(panel[1], panel[2], panel[3], panel[4], panel[5], panel[6], panel[7]))
    table.insert(xml, rect_shape(panel[1] + 1, panel[2] .. " rail", panel[3], panel[4], 4, panel[6], panel[8]))
  end
  return table.concat(xml)
end

local function blocks_to_text(blocks)
  return pandoc.utils.stringify(pandoc.Div(blocks or {}))
end

local function starts_with_image(block)
  if block.t ~= "Para" and block.t ~= "Plain" then return false end
  local first = block.content and block.content[1]
  if not first then return false end
  if first.t == "Image" then return true end
  return first.t == "Link" and first.content and first.content[1] and first.content[1].t == "Image"
end

local function is_notes_div(block)
  return block.t == "Div" and has_class(block, "notes")
end

local function is_figure_notes(block)
  return block.t == "Div" and has_class(block, "figure-notes")
end

local function columns_kind(block, class)
  return block.t == "Div" and has_class(block, "columns") and has_class(block, class)
end

local function append_blocks(target, source)
  for _, block in ipairs(source or {}) do target:insert(block) end
end

local function meta_inlines(value)
  if value == nil then return pandoc.Inlines({}) end
  if pandoc.utils.type(value) == "Inlines" then return value:clone() end
  if type(value) == "table" and value.t == "MetaInlines" then return pandoc.Inlines(value) end
  local text = pandoc.utils.stringify(value)
  if text == "" then return pandoc.Inlines({}) end
  return pandoc.Inlines({pandoc.Str(text)})
end

local function merge_institute(meta)
  if meta.institute == nil then return end
  local institute = meta_inlines(meta.institute)
  if #institute == 0 then return end
  local author = meta_inlines(meta.author)
  if #author > 0 then author:insert(pandoc.LineBreak()) end
  for _, inline in ipairs(institute) do author:insert(inline) end
  meta.author = author
  meta.institute = nil
end

local function section_header(header)
  for _, class in ipairs(header.classes or {}) do
    local filename = section_backgrounds[class]
    if filename then
      header.level = 1
      header.attributes["background-image"] = pandoc.path.join({backgrounds_dir, filename})
      return header
    end
  end
  return nil
end

local function unwrap_equation_panels(blocks)
  local out = pandoc.Blocks({})
  for _, block in ipairs(blocks or {}) do
    if block.t == "Div" and has_class(block, "equation-panel") then
      append_blocks(out, block.content)
    else
      out:insert(block)
    end
  end
  return out
end

local function transform_method_columns(columns)
  local left = columns.content and columns.content[1]
  local right = columns.content and columns.content[2]
  if not left or not right or left.t ~= "Div" or right.t ~= "Div" then return columns end
  left.content = unwrap_equation_panels(left.content)
  right.content = unwrap_equation_panels(right.content)
  left.content:insert(1, pandoc.RawBlock("openxml", method_panel_shapes()))
  return columns
end

local function publication_item_inlines(item, number)
  local out = pandoc.Inlines({raw_run(tostring(number) .. ". ", {color = "000000", size = 820})})
  local first_block = true
  for _, block in ipairs(item or {}) do
    if block.t == "Para" or block.t == "Plain" then
      if not first_block then out:insert(raw_run(" ", {size = 820})) end
      for _, inline in ipairs(block.content) do out:insert(inline) end
      first_block = false
    end
  end
  return out
end

local function transform_publication_inventory(div)
  for _, column in ipairs(div.content or {}) do
    if column.t == "Div" and has_class(column, "column") then
      local out = pandoc.Blocks({})
      for _, block in ipairs(column.content or {}) do
        if block.t == "OrderedList" then
          local start = 1
          if block.listAttributes and block.listAttributes[1] then start = block.listAttributes[1] end
          for index, item in ipairs(block.content) do
            out:insert(pandoc.Para(publication_item_inlines(item, start + index - 1)))
          end
        else
          out:insert(block)
        end
      end
      column.content = out
    end
  end
  return div
end

local function item_inlines(blocks)
  for _, block in ipairs(blocks or {}) do
    if block.t == "Para" or block.t == "Plain" then return block.content end
  end
  return pandoc.Inlines({pandoc.Str(blocks_to_text(blocks))})
end

local function transform_spike_grid(div)
  local right = div.content and div.content[2]
  if not right or right.t ~= "Div" then return div end
  local out = pandoc.Blocks({})
  for _, block in ipairs(right.content or {}) do
    if block.t == "Header" then
      out:insert(pandoc.Para(render_inlines(block.content, {color = palette.pine, size = 900, bold = true})))
    elseif block.t == "Div" and has_class(block, "spike-item") then
      local inlines = pandoc.Inlines({raw_run("▎ ", {color = palette.gold, size = 900, bold = true})})
      local styled = render_inlines(item_inlines(block.content), {color = palette.text, size = 850})
      for _, inline in ipairs(styled) do inlines:insert(inline) end
      out:insert(pandoc.Para(inlines))
    else
      out:insert(block)
    end
  end
  right.content = out
  return div
end

local function transform_semantic_div(div)
  if has_class(div, "method-equations") and has_class(div, "columns") then
    return transform_method_columns(div)
  elseif has_class(div, "publication-inventory") and has_class(div, "columns") then
    return transform_publication_inventory(div)
  elseif has_class(div, "spike-grid") and has_class(div, "columns") then
    return transform_spike_grid(div)
  end
  return nil
end

local function small_note_para(div, size)
  local text = blocks_to_text(div.content)
  return pandoc.Para({raw_run(text, {color = palette.subtle, size = size or 600, italic = true})})
end

local function append_method_figure_note(columns, note)
  local right = columns.content and columns.content[2]
  if not right or right.t ~= "Div" then return end
  right.content:insert(small_note_para(note, 580))
end

local function append_spike_figure_note(columns, note)
  local right = columns.content and columns.content[2]
  if not right or right.t ~= "Div" then return end
  local text = blocks_to_text(note.content)
  right.content:insert(pandoc.RawBlock("openxml", text_shape(470, "wfmR figure note", 154, 606, 972, 28, text, {color = palette.subtle, size = 560, italic = true})))
end

local function style_budget_block(block)
  if is_figure_notes(block) then
    return pandoc.Para({raw_run(blocks_to_text(block.content), {color = palette.subtle, size = 600, italic = true})})
  elseif block.t == "Div" and has_class(block, "policy-rail") then
    local inlines = item_inlines(block.content)
    return pandoc.Para(render_inlines(inlines, {color = palette.subtle, size = 780}))
  elseif block.t == "BlockQuote" then
    local inlines = item_inlines(block.content)
    return pandoc.Para(render_inlines(inlines, {color = palette.text, size = 980, bold = true}))
  end
  return block
end

local function transform_budget_segment(segment)
  if #segment < 2 or not starts_with_image(segment[2]) then return nil end
  local visible = pandoc.Blocks({})
  local notes = pandoc.Blocks({})
  for index = 3, #segment do
    local block = segment[index]
    if is_notes_div(block) then
      notes:insert(block)
    else
      visible:insert(style_budget_block(block))
    end
  end
  local out = pandoc.Blocks({segment[1]})
  append_blocks(out, visible)
  out:insert(segment[2])
  append_blocks(out, notes)
  return out
end

local function split_references(segment)
  if not has_class(segment[1], "smaller") then return nil end
  for index = 2, #segment do
    local block = segment[index]
    if block.t == "Div" and block.identifier == "refs" and #block.content > 1 then
      local midpoint = math.ceil(#block.content / 2)
      local left_blocks = pandoc.Blocks({})
      local right_blocks = pandoc.Blocks({})
      for entry_index, entry in ipairs(block.content) do
        if entry_index <= midpoint then left_blocks:insert(entry) else right_blocks:insert(entry) end
      end
      local columns = pandoc.Div({
        pandoc.Div(left_blocks, pandoc.Attr("", {"column"}, {width = "49%"})),
        pandoc.Div(right_blocks, pandoc.Attr("", {"column"}, {width = "49%"}))
      }, pandoc.Attr("", {"columns"}, {}))
      local out = pandoc.Blocks({segment[1]})
      for j = 2, #segment do
        if j == index then out:insert(columns) else out:insert(segment[j]) end
      end
      return out
    end
  end
  return nil
end

local function transform_columns_segment(segment)
  if #segment < 2 or segment[2].t ~= "Div" or not has_class(segment[2], "columns") then return nil end
  local columns = segment[2]
  local figure_note = nil
  local notes = pandoc.Blocks({})
  local other = pandoc.Blocks({})
  for index = 3, #segment do
    local block = segment[index]
    if is_figure_notes(block) then
      figure_note = block
    elseif is_notes_div(block) then
      notes:insert(block)
    else
      other:insert(block)
    end
  end
  if not figure_note then return nil end
  if has_class(columns, "method-equations") then
    append_method_figure_note(columns, figure_note)
  elseif has_class(columns, "spike-grid") then
    append_spike_figure_note(columns, figure_note)
  else
    local right = columns.content and columns.content[2]
    if right and right.t == "Div" then right.content:insert(small_note_para(figure_note, 600)) end
  end
  local out = pandoc.Blocks({segment[1], columns})
  append_blocks(out, other)
  append_blocks(out, notes)
  return out
end

local function restructure_slides(blocks)
  local out = pandoc.Blocks({})
  local index = 1
  while index <= #blocks do
    local block = blocks[index]
    if block.t ~= "Header" or block.level > 2 then
      out:insert(block)
      index = index + 1
    else
      local segment = pandoc.Blocks({block})
      index = index + 1
      while index <= #blocks and not (blocks[index].t == "Header" and blocks[index].level <= 2) do
        segment:insert(blocks[index])
        index = index + 1
      end
      local transformed = transform_columns_segment(segment)
        or transform_budget_segment(segment)
        or split_references(segment)
        or segment
      append_blocks(out, transformed)
    end
  end
  return out
end

function Pandoc(doc)
  -- Resolve citations before the layout pass so the bibliography can be split
  -- into native PowerPoint columns. Quarto's default citeproc pass runs after
  -- user filters, which otherwise leaves the References slide empty here.
  if doc.meta.bibliography ~= nil then
    doc = pandoc.utils.citeproc(doc)
  end
  merge_institute(doc.meta)
  doc = doc:walk({Math = strip_math_colour})
  doc = doc:walk({Header = section_header})
  doc = doc:walk({Div = transform_semantic_div})
  doc.blocks = restructure_slides(doc.blocks)
  doc = doc:walk({Span = span_to_native})
  return doc
end
