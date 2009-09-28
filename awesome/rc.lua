-- Include awesome libraries, with lots of useful function!
require("awful")
require("beautiful")
require("wicked")
require("naughty")

-- {{{ Variable definitions
-- Themes define colours, icons, and wallpapers
-- The default is a dark theme
theme_path = "/usr/share/awesome/themes/default/theme"
-- Uncommment this for a lighter theme
-- theme_path = "/usr/share/awesome/themes/sky/theme"

-- Actually load theme
beautiful.init(theme_path)

-- This is used later as the default terminal and editor to run.
terminal = "gnome-terminal"
editor = os.getenv("EDITOR") or "emacs"
editor_cmd = editor
browser = "firefox"
lock_screen = "gnome-screensaver-command -l"

-- Default modkey.
-- Usually, Mod4 is the key with a logo between Control and Alt.
-- If you do not like this or do not have such a key,
-- I suggest you to remap Mod4 to another key using xmodmap or other tools.
-- However, you can use another modifier like Mod1, but it may interact with others.
modkey = "Mod4"

main_screen = 2
side_screen = 1

-- Table of layouts to cover with awful.layout.inc, order matters.
layouts =
{
    "tile",
    "tileleft",
    "tilebottom",
    "tiletop",
    "fairh",
    "fairv",
    "magnifier",
    "max",
    "fullscreen",
    "spiral",
    "dwindle",
    "floating"
}

-- Table of clients that should be set floating. The index may be either
-- the application class or instance. The instance is useful when running
-- a console app in a terminal like (Music on Console)
--    xterm -name mocp -e mocp
floatapps =
{
    -- by class
    ["MPlayer"] = true,
    ["pinentry"] = true,
    ["gimp"] = true,
    -- by instance
    ["mocp"] = true
}

-- Applications to be moved to a pre-defined tag by class or instance.
-- Use the screen and tags indices.
apptags =
{
	["pidgin"] = { screen = side_screen, tag = 2},
	["gmpc"] = { screen = side_screen, tag = 2},
	["gnome-volume-control"] = { screen = side_screen, tag = 2},
	["gnome-terminal"] = { screen = main_screen, tag = 1},
	["emacs"] = { screen = main_screen, tag = 2},
    ["Firefox"] = { screen = main_screen, tag = 4 },
    ["chromium-browser"] = { screen = main_screen, tag = 3 },
}

-- Define if we want to use titlebar on all applications.
use_titlebar = false
-- }}}

-- {{{ Tags
-- Define tags table.
main_screen_tags = {"shell", "emacs", "web", "selenium"}
side_screen_tags = {"emacs", "music"}
custom_tag_names_by_screen = {}
custom_tag_names_by_screen[main_screen] = main_screen_tags
custom_tag_names_by_screen[side_screen] = side_screen_tags

tags = {}
for s = 1, screen.count() do
    -- Each screen has its own tag table.
    tags[s] = {}
    -- Create 9 tags per screen.
    for tagnumber = 1, #custom_tag_names_by_screen[s] do
	   tagname = custom_tag_names_by_screen[s][tagnumber]
	   tags[s][tagnumber] = tag({ name = tagname, layout = layouts[1] })
	   -- Add tags to screen one by one
	   tags[s][tagnumber].screen = s
    end
    -- I'm sure you want to see at least one tag.
    tags[s][1].selected = true
end
-- }}}

-- {{{ Naughty boxes

-- Calculator
val = nil
keybinding({ modkey}, "c", 
		   function ()
			  awful.prompt.run({ prompt = "Calc: "
							   }, mainpromptbox,
							   function(expr)
								  val = awful.util.eval(expr)
								  naughty.notify({ text = expr .. ' = <span color="white">' .. val .. "</span>",
													timeout = 5,
													run = function() 
															 io.popen("echo ".. val .. " | xsel -i"):close()
														  end, })
							   end,
							   nil, 
							   awful.util.getdir("cache") .. "/calc")
		   end):add()
-- }}}

-- {{{ Wibox

-- Create a systray
mysystray = widget({ type = "systray", align = "right" })

-- Create wicked widgets

--twitterbox = widget({ type = 'textbox', name = 'twitterbox', align = 'right'})
-- function last_tweet()
--    local filedescriptor = io.popen('/home/mattj/bin/tweets.py')
--    local value = filedescriptor:read()
--    filedescriptor:close()

--    return {value}
-- end
-- wicked.register(twitterbox, last_tweet, "$1", 120)

-- twitterbox.mouse_enter = function () 
-- 		local f = io.popen("mpc playlist | egrep '^>' -A3 -B3")

-- 		playing = playing .. '<span color="white">Playlist:</span>\n'

-- 		for line in f:lines() do
-- 			playing = playing .. line .. '\n' 
-- 		end
-- 		f:close()

-- 		tweets = naughty.notify({ text = playing,
-- 									 timeout = 0, hover_timeout = 0.5,
-- 									 screen = main_screen
-- 								  })
-- end
-- twitterbox.mouse_leave = function () naughty.destroy(tweets) end

-- Create wicked widgets
mpdbox = widget({ type = 'textbox', name = 'mpdbox', align = 'right'})

-- {{{ 
-- Fancy mpd control widget
mpdbox:buttons({  button({ }, 1, function() 
									awful.util.pread("mpc toggle") 
								 end),
				  button({ }, 3, function()
									awful.util.pread("mpc next")
									mpdbox.mouse_leave()
									mpdbox.mouse_enter()
								 end),
				  button({ modkey }, 3, function()
										   awful.util.pread("mpc prev")
										   mpdbox.mouse_leave()
										   mpdbox.mouse_enter()
										end),
				  button({ }, 4, function()
									awful.util.pread("amixer sset 'Master',0 3+")
									mpdbox.mouse_leave()
									mpdbox.mouse_enter()
								 end),
				  button({ }, 5, function()
									awful.util.pread("amixer sset 'Master',0 3-")
									mpdbox.mouse_leave()
									mpdbox.mouse_enter()
								 end),
			   })
									
wicked.register(mpdbox, wicked.widgets.mpd, 
						function (widget, args)
						         if args[1] ~= '' and args[1]:find("volume:") == nil then
								   return '  <span color="white">Now Playing:</span> ' .. args[1] .. ' '
								 else
								   return '  <span color="white">MPD Stopped</span> '
								 end
						end)

local playlist = nil

mpdbox.mouse_enter = function () 
		local playing = ""

		local f = io.popen("mpc playlist | egrep '^>' -A3 -B3")

		playing = playing .. '<span color="white">Playlist:</span>\n'

		for line in f:lines() do
			playing = playing .. line .. '\n' 
		end
		f:close()

		playlist = naughty.notify({ text = playing,
									 timeout = 0, hover_timeout = 0.5,
									 screen = main_screen
								  })
end
mpdbox.mouse_leave = function () naughty.destroy(playlist) end

--}}}
		
--{{{ Calendar

datebox = widget({ type = 'textbox', name = 'datebox', align = 'right'})
wicked.register(datebox, wicked.widgets.date, '  <span color="white">Date: </span> %l:%M %P %a %D  ')

local calendar = nil
datebox.mouse_enter = function () 
		local f = io.popen("cal -m")
		local cal = "\n"
		local i = 1
		for line in f:lines() do
			if i > 1 then
			cal = cal .. "\n" .. line 
			end
			i = i + 1
		end
		f:close()
		calendar = naughty.notify({ text = os.date("%a, %d %B %Y") .. cal, 
									 timeout = 0, hover_timeout = 0.5,
									 width = 150, screen = main_screen
								  })
end
datebox.mouse_leave = function () naughty.destroy(calendar) end

--}}}

-- Create a launcher widget and a main menu
myawesomemenu = {
   { "manual", terminal .. " -e man awesome" },
   { "edit config", editor_cmd .. " " .. awful.util.getdir("config") .. "/rc.lua" },
   { "restart", awesome.restart },
   { "quit", awesome.quit }
}

-- yelp dev stuff
yelp_site_menu = {
   { "dev", browser .. " http://mattj.dev.yelp.com" },
   { "dev admin", browser .. " http://admin.mattj.dev.yelp.com" },
   { "live admin", browser .. " http://admin.yelp.com" },
   { "live", browser .. " http://yelp.com" },
   { "stage", browser .. " http://stage.yelp.com" }
}

mymainmenu = awful.menu.new({ items = { { "awesome", myawesomemenu, beautiful.awesome_icon },
		   	 				  		    { "pydoc", browser .. " http://www.python.org/doc/2.5.2/lib/lib.html" },
										{ "yelp", yelp_site_menu },
										{ "lock", lock_screen },
                                        { "open terminal", terminal }
                                      }
                            })

mylauncher = awful.widget.launcher({ image = beautiful.awesome_icon,
                                     menu = mymainmenu })

-- Create a wibox for each screen and add it
mywibox = {}
mypromptbox = {}
mylayoutbox = {}
mytaglist = {}
mytaglist.buttons = { button({ }, 1, awful.tag.viewonly),
                      button({ modkey }, 1, awful.client.movetotag),
                      button({ }, 3, function (tag) tag.selected = not tag.selected end),
                      button({ modkey }, 3, awful.client.toggletag),
                      button({ }, 4, awful.tag.viewnext),
                      button({ }, 5, awful.tag.viewprev) }
mytasklist = {}
mytasklist.buttons = { button({ }, 1, function (c) client.focus = c; c:raise() end),
                       button({ }, 3, function () awful.menu.clients({ width=250 }) end),
                       button({ }, 4, function () awful.client.focus.byidx(1) end),
                       button({ }, 5, function () awful.client.focus.byidx(-1) end) }

for s = 1, screen.count() do
    -- Create a promptbox for each screen
    mypromptbox[s] = widget({ type = "textbox", align = "left" })

    -- Create an imagebox widget which will contains an icon indicating which layout we're using.
    -- We need one layoutbox per screen.
    mylayoutbox[s] = widget({ type = "imagebox", align = "right" })
    mylayoutbox[s]:buttons({ button({ }, 1, function () awful.layout.inc(layouts, 1) end),
                             button({ }, 3, function () awful.layout.inc(layouts, -1) end),
                             button({ }, 4, function () awful.layout.inc(layouts, 1) end),
                             button({ }, 5, function () awful.layout.inc(layouts, -1) end) })
    -- Create a taglist widget
    mytaglist[s] = awful.widget.taglist.new(s, awful.widget.taglist.label.all, mytaglist.buttons)

    -- Create a tasklist widget
    mytasklist[s] = awful.widget.tasklist.new(function(c)
                                                  return awful.widget.tasklist.label.currenttags(c, s)
                                              end, mytasklist.buttons)

    -- Create the wibox
    mywibox[s] = wibox({ position = "top", fg = beautiful.fg_normal, bg = beautiful.bg_normal })
    -- Add widgets to the wibox - order matters
	if s == main_screen then
	   mywibox[s].widgets = { mylauncher, mytaglist[s], mytasklist[s],
		  mypromptbox[s], mpdbox, datebox, mysystray, mylayoutbox[s] }
	else
	   mywibox[s].widgets = { mytaglist[s], mytasklist[s], mypromptbox[s], mylayoutbox[s] } --twitterbox, mylayoutbox[s] }
	end

    mywibox[s].screen = s
end
-- }}}

mainpromptbox = mypromptbox[main_screen]

-- {{{ Mouse bindings
awesome.buttons({
    button({ }, 3, function () mymainmenu:toggle() end),
    button({ }, 4, awful.tag.viewnext),
    button({ }, 5, awful.tag.viewprev)
})
-- }}}

-- {{{ Key bindings

-- Bind keyboard digits
-- Compute the maximum number of digit we need, limited to 9
keynumber = 0
for s = 1, screen.count() do
   keynumber = math.min(9, math.max(#tags[s], keynumber));
end

for i = 1, keynumber do
    keybinding({ modkey }, i,
                   function ()
                       local screen = mouse.screen
                       if tags[screen][i] then
                           awful.tag.viewonly(tags[screen][i])
                       end
                   end):add()
    keybinding({ modkey, "Control" }, i,
                   function ()
                       local screen = mouse.screen
                       if tags[screen][i] then
                           tags[screen][i].selected = not tags[screen][i].selected
                       end
                   end):add()
    keybinding({ modkey, "Shift" }, i,
                   function ()
                       if client.focus then
                           if tags[client.focus.screen][i] then
                               awful.client.movetotag(tags[client.focus.screen][i])
                           end
                       end
                   end):add()
    keybinding({ modkey, "Control", "Shift" }, i,
                   function ()
                       if client.focus then
                           if tags[client.focus.screen][i] then
                               awful.client.toggletag(tags[client.focus.screen][i])
                           end
                       end
                   end):add()
end

keybinding({ modkey }, "Left", awful.tag.viewprev):add()
keybinding({ modkey }, "Right", awful.tag.viewnext):add()
keybinding({ modkey }, "Escape", awful.tag.history.restore):add()

-- Standard programs
keybinding({ modkey }, "Return", function () awful.util.spawn(terminal) end):add()
keybinding({ modkey }, "BackSpace", function () awful.util.spawn(editor) end):add()
keybinding({ modkey }, "l", function () awful.util.spawn(lock_screen) end):add()

keybinding({ modkey, "Control" }, "r", function ()
                                           mypromptbox[mouse.screen].text =
                                               awful.util.escape(awful.util.restart())
                                        end):add()
keybinding({ modkey, "Shift" }, "q", awesome.quit):add()

-- Client manipulation
keybinding({ modkey }, "m", awful.client.maximize):add()
keybinding({ modkey }, "f", function () if client.focus then client.focus.fullscreen = not client.focus.fullscreen end end):add()
keybinding({ modkey, "Shift" }, "c", function () if client.focus then client.focus:kill() end end):add()
keybinding({ modkey }, "j", function () awful.client.focus.byidx(1); if client.focus then client.focus:raise() end end):add()
keybinding({ modkey }, "k", function () awful.client.focus.byidx(-1);  if client.focus then client.focus:raise() end end):add()
keybinding({ modkey, "Shift" }, "j", function () awful.client.swap.byidx(1) end):add()
keybinding({ modkey, "Shift" }, "k", function () awful.client.swap.byidx(-1) end):add()
keybinding({ modkey, "Control" }, "j", function () awful.screen.focus(1) end):add()
keybinding({ modkey, "Control" }, "k", function () awful.screen.focus(-1) end):add()
keybinding({ modkey, "Control" }, "space", awful.client.togglefloating):add()
keybinding({ modkey, "Control" }, "Return", function () if client.focus then client.focus:swap(awful.client.getmaster()) end end):add()
keybinding({ modkey }, "o", awful.client.movetoscreen):add()
keybinding({ modkey }, "Tab", awful.client.focus.history.previous):add()
keybinding({ modkey }, "u", awful.client.urgent.jumpto):add()
keybinding({ modkey, "Shift" }, "r", function () if client.focus then client.focus:redraw() end end):add()

-- Layout manipulation
keybinding({ modkey }, "l", function () awful.tag.incmwfact(0.05) end):add()
keybinding({ modkey }, "h", function () awful.tag.incmwfact(-0.05) end):add()
keybinding({ modkey, "Shift" }, "h", function () awful.tag.incnmaster(1) end):add()
keybinding({ modkey, "Shift" }, "l", function () awful.tag.incnmaster(-1) end):add()
keybinding({ modkey, "Control" }, "h", function () awful.tag.incncol(1) end):add()
keybinding({ modkey, "Control" }, "l", function () awful.tag.incncol(-1) end):add()
keybinding({ modkey }, "space", function () awful.layout.inc(layouts, 1) end):add()
keybinding({ modkey, "Shift" }, "space", function () awful.layout.inc(layouts, -1) end):add()

-- Prompt
keybinding({ modkey }, "F2", function ()
								awful.prompt.run({ prompt = "Run: " },
												 mypromptbox[mouse.screen], 
												 awful.util.spawn, awful.completion.bash,
												 awful.util.getdir("cache") .. "/history")
                             end):add()
keybinding({ modkey }, "F4", function ()
								awful.prompt.run({ prompt = "Run Lua code: " }, mypromptbox[mouse.screen], awful.util.eval, awful.prompt.bash,
												 awful.util.getdir("cache") .. "/history_eval")
                             end):add()

keybinding({ modkey, "Ctrl" }, "i", function ()
                                        local s = mouse.screen
                                        if mypromptbox[s].text then
                                            mypromptbox[s].text = nil
                                        elseif client.focus then
                                            mypromptbox[s].text = nil
                                            if client.focus.class then
                                                mypromptbox[s].text = "Class: " .. client.focus.class .. " "
                                            end
                                            if client.focus.instance then
                                                mypromptbox[s].text = mypromptbox[s].text .. "Instance: ".. client.focus.instance .. " "
                                            end
                                            if client.focus.role then
                                                mypromptbox[s].text = mypromptbox[s].text .. "Role: ".. client.focus.role
                                            end
                                        end
                                    end):add()

-- Client awful tagging: this is useful to tag some clients and then do stuff like move to tag on them
keybinding({ modkey }, "t", awful.client.togglemarked):add()

for i = 1, keynumber do
    keybinding({ modkey, "Shift" }, "F" .. i,
                   function ()
                       local screen = mouse.screen
                       if tags[screen][i] then
                           for k, c in pairs(awful.client.getmarked()) do
                               awful.client.movetotag(tags[screen][i], c)
                           end
                       end
                   end):add()
end
-- }}}

-- {{{ Hooks
-- Hook function to execute when focusing a client.
awful.hooks.focus.register(function (c)
    if not awful.client.ismarked(c) then
        c.border_color = beautiful.border_focus
    end
end)

-- Hook function to execute when unfocusing a client.
awful.hooks.unfocus.register(function (c)
    if not awful.client.ismarked(c) then
        c.border_color = beautiful.border_normal
    end
end)

-- Hook function to execute when marking a client
awful.hooks.marked.register(function (c)
    c.border_color = beautiful.border_marked
end)

-- Hook function to execute when unmarking a client.
awful.hooks.unmarked.register(function (c)
    c.border_color = beautiful.border_focus
end)

-- Hook function to execute when the mouse enters a client.
awful.hooks.mouse_enter.register(function (c)
    -- Sloppy focus, but disabled for magnifier layout
    if awful.layout.get(c.screen) ~= "magnifier"
        and awful.client.focus.filter(c) then
        client.focus = c
    end
end)

-- Hook function to execute when a new client appears.
awful.hooks.manage.register(function (c)
    if use_titlebar then
        -- Add a titlebar
        awful.titlebar.add(c, { modkey = modkey })
    end
    -- Add mouse bindings
    c:buttons({
        button({ }, 1, function (c) client.focus = c; c:raise() end),
        button({ modkey }, 1, function (c) c:mouse_move() end),
        button({ modkey }, 3, function (c) c:mouse_resize() end)
    })
    -- New client may not receive focus
    -- if they're not focusable, so set border anyway.
    c.border_width = beautiful.border_width
    c.border_color = beautiful.border_normal

    -- Check if the application should be floating.
    local cls = c.class
    local inst = c.instance
    if floatapps[cls] then
        c.floating = floatapps[cls]
    elseif floatapps[inst] then
        c.floating = floatapps[inst]
    end

    -- Check application->screen/tag mappings.
    local target
    if apptags[cls] then
        target = apptags[cls]
    elseif apptags[inst] then
        target = apptags[inst]
    end
    if target then
        c.screen = target.screen
        awful.client.movetotag(tags[target.screen][target.tag], c)
    end

    -- Do this after tag mapping, so you don't see it on the wrong tag for a split second.
    client.focus = c

    -- Set the windows at the slave,
    -- i.e. put it at the end of others instead of setting it master.
    -- awful.client.setslave(c)

    -- Honor size hints: if you want to drop the gaps between windows, set this to false.
    -- c.honorsizehints = false
end)

-- Hook function to execute when arranging the screen.
-- (tag switch, new client, etc)
awful.hooks.arrange.register(function (screen)
    local layout = awful.layout.get(screen)
    if layout then
        mylayoutbox[screen].image = image(beautiful["layout_" .. layout])
    else
        mylayoutbox[screen].image = nil
    end

    -- Give focus to the latest client in history if no window has focus
    -- or if the current window is a desktop or a dock one.
    if not client.focus then
        local c = awful.client.focus.history.get(screen, 0)
        if c then client.focus = c end
    end

    -- Uncomment if you want mouse warping
    --[[
    if client.focus then
        local c_c = client.focus:fullgeometry()
        local m_c = mouse.coords()

        if m_c.x < c_c.x or m_c.x >= c_c.x + c_c.width or
            m_c.y < c_c.y or m_c.y >= c_c.y + c_c.height then
            if table.maxn(m_c.buttons) == 0 then
                mouse.coords({ x = c_c.x + 5, y = c_c.y + 5})
            end
        end
    end
    ]]
end)

-- }}}
