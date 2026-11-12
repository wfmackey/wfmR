-- Sixth-level headings run into the paragraph that follows them, set as
-- "_Heading_.---Paragraph text", in every output format. The heading's
-- identifier stays on the italic text so links to it still work.
local function lead(header)
  return pandoc.Span(
    { pandoc.Emph(header.content) },
    pandoc.Attr(header.identifier)
  )
end

function Blocks(blocks)
  local out = pandoc.List()
  local i = 1
  while i <= #blocks do
    local block = blocks[i]
    local nxt = blocks[i + 1]
    if block.t == "Header" and block.level == 6 then
      if nxt and (nxt.t == "Para" or nxt.t == "Plain") then
        local inlines = pandoc.List({ lead(block), pandoc.Str(".—") })
        inlines:extend(nxt.content)
        out:insert(pandoc.Para(inlines))
        i = i + 2
      else
        out:insert(pandoc.Para({ lead(block), pandoc.Str(".") }))
        i = i + 1
      end
    else
      out:insert(block)
      i = i + 1
    end
  end
  return out
end
