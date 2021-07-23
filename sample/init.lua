-- Example.
local docs = [[
  Long format string here
]]

local more_docs = [==[
  Note this one can even include "]]" in it without terminating the string.
]==]

if true then
  print('hello' .. "world")
  print(docs)
  print(more_docs)
end

--[[
print('Commented-out code')
--]]
