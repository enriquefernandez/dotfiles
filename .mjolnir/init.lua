local application = require "mjolnir.application"
local hotkey = require "mjolnir.hotkey"
local window = require "mjolnir.window"
local fnutils = require "mjolnir.fnutils"
local spotify     = require "mjolnir.lb.spotify"
local alert = require "mjolnir.alert"

local mash      = {"cmd", "alt", "ctrl"}
local mashshift = {"cmd", "alt", "shift"}
local hyper = {"cmd", "alt", "ctrl", "shift"}

hotkey.bind({"cmd", "alt", "ctrl"}, "D", function()
	  local win = window.focusedwindow()
	  local f = win:frame()
	  f.x = f.x + 10
	  win:setframe(f)
end)

local hints = require "mjolnir.th.hints"
hotkey.bind({"shift", "ctrl","cmd", "alt"},"e",hints.windowHints)
-- You can also use this with appfinder to switch to windows of a specific app
local appfinder = require "mjolnir.cmsj.appfinder"
hotkey.bind({"shift", "ctrl","cmd", "alt"},"k",function() hints.appHints(appfinder.app_from_name("Emacs")) end)

hotkey.bind(hyper, 'space', spotify.displayCurrentTrack)

hotkey.bind(hyper, 'm', function()
			   alert.show("Mjolnir, keybinding!.", 3)
end)

alert.show("Mjolnir, at your service.", 3)
