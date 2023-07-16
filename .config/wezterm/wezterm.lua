local wezterm = require 'wezterm'
local act = wezterm.action
local colors = require('lua/rose-pine-dawn').colors()
local window_frame = require('lua/rose-pine-dawn').window_frame()


local config = {}

-- config.font = wezterm.font('MonoLisa', { weight = Light})
config.font =wezterm.font('Source Code Pro', {weight = Light})
config.font_size = 11
-- config.color_scheme = "Catppuccin Mocha"
config.colors = colors
config.line_height = 0.9
config.adjust_window_size_when_changing_font_size = false
config.default_cursor_style = 'BlinkingBar'
config.animation_fps = 1
config.cursor_blink_ease_in = 'Constant'
config.cursor_blink_ease_out = 'Constant'
config.enable_tab_bar = false
config.tab_bar_at_bottom = false
config.show_tab_index_in_tab_bar = true
config.tab_max_width = 56
config.use_fancy_tab_bar = false
config.initial_cols = 98
config.initial_rows = 24
config.window_padding = {
  left = 40,
  right = 40,
  top = 40,
  bottom = 40,
}


-- Show which key table is active in the status area
wezterm.on('update-right-status', function(window, pane)
  local name = window:active_key_table()
  if name then
    name = 'TABLE: ' .. name
  end
  window:set_right_status(name or '')
end)

config.leader = { key = 'Space', mods = 'CTRL|SHIFT' }
config.keys = {
  -- CTRL+SHIFT+Space, followed by 'r' will put us in resize-pane
  -- mode until we cancel that mode.
  {
    key = 'r',
    mods = 'LEADER',
    action = act.ActivateKeyTable {
      name = 'resize_pane',
      one_shot = false,
    },
  },

  -- CTRL+SHIFT+Space, followed by 'a' will put us in activate-pane
  -- mode until we press some other key or until 1 second (1000ms)
  -- of time elapses
  {
    key = 'a',
    mods = 'LEADER',
    action = act.ActivateKeyTable {
      name = 'activate_pane',
      timeout_milliseconds = 1000,
    },
  },


  -- ScrollByLine
  { key = 'UpArrow', mods = 'SHIFT', action = act.ScrollByLine(-1) },
  { key = 'DownArrow', mods = 'SHIFT', action = act.ScrollByLine(1) },

}

config.key_tables = {
  -- Defines the keys that are active in our resize-pane mode.
  -- Since we're likely to want to make multiple adjustments,
  -- we made the activation one_shot=false. We therefore need
  -- to define a key assignment for getting out of this mode.
  -- 'resize_pane' here corresponds to the name="resize_pane" in
  -- the key assignments above.
  resize_pane = {
    { key = 'LeftArrow', action = act.AdjustPaneSize { 'Left', 1 } },
    { key = 'h', action = act.AdjustPaneSize { 'Left', 1 } },

    { key = 'RightArrow', action = act.AdjustPaneSize { 'Right', 1 } },
    { key = 'l', action = act.AdjustPaneSize { 'Right', 1 } },

    { key = 'UpArrow', action = act.AdjustPaneSize { 'Up', 1 } },
    { key = 'k', action = act.AdjustPaneSize { 'Up', 1 } },

    { key = 'DownArrow', action = act.AdjustPaneSize { 'Down', 1 } },
    { key = 'j', action = act.AdjustPaneSize { 'Down', 1 } },

    -- Cancel the mode by pressing escape
    { key = 'Escape', action = 'PopKeyTable' },
  },

  -- Defines the keys that are active in our activate-pane mode.
  -- 'activate_pane' here corresponds to the name="activate_pane" in
  -- the key assignments above.
  activate_pane = {
    { key = 'LeftArrow', action = act.ActivatePaneDirection 'Left' },
    { key = 'h', action = act.ActivatePaneDirection 'Left' },

    { key = 'RightArrow', action = act.ActivatePaneDirection 'Right' },
    { key = 'l', action = act.ActivatePaneDirection 'Right' },

    { key = 'UpArrow', action = act.ActivatePaneDirection 'Up' },
    { key = 'k', action = act.ActivatePaneDirection 'Up' },

    { key = 'DownArrow', action = act.ActivatePaneDirection 'Down' },
    { key = 'j', action = act.ActivatePaneDirection 'Down' },
  },
}

-- config.colors = {
--   -- The default text color
--   foreground = '#bbc2cf',
--   -- The default background color
--   background = '#282c34',

--   -- Overrides the cell background color when the current cell is occupied by the
--   -- cursor and the cursor style is set to Block
--   cursor_bg = '#528bff',
--   -- Overrides the text color when the current cell is occupied by the cursor
--   -- cursor_fg = 'black',
--   -- Specifies the border color of the cursor when the cursor style is set to Block,
--   -- or the color of the vertical or horizontal bar when the cursor style is set to
--   -- Bar or Underline.
--   -- cursor_border = '#52ad70',

--   -- the foreground color of selected text
--   -- selection_fg = 'black',
--   -- the background color of selected text
--   -- selection_bg = '#fffacd',

--   -- The color of the scrollbar "thumb"; the portion that represents the current viewport
--   -- scrollbar_thumb = '#222222',

--   -- The color of the split lines between panes
--   split = '#1c1f24',

--   ansi = {
--     '#1c1f24',
--     '#ff6c6b',
--     '#98be65',
--     '#da8548',
--     '#51afef',
--     '#c678dd',
--     '#5699af',
--     '#abb2bf',
--   },
--   brights = {
--     '#5b6268',
--     '#da8548',
--     '#4db5bd',
--     '#ecbe7b',
--     '#3071db',
--     '#a9a1e1',
--     '#46d9ff',
--     '#dfdfdf',
--   },

--   -- Arbitrary colors of the palette in the range from 16 to 255
--   -- indexed = { [136] = '#af8700' },

--   -- Since: 20220319-142410-0fcdea07
--   -- When the IME, a dead key or a leader key are being processed and are effectively
--   -- holding input pending the result of input composition, change the cursor
--   -- to this color to give a visual cue about the compose state.
--   -- compose_cursor = 'orange',

--   -- Colors for copy_mode and quick_select
--   -- available since: 20220807-113146-c2fee766
--   -- In copy_mode, the color of the active text is:
--   -- 1. copy_mode_active_highlight_* if additional text was selected using the mouse
--   -- 2. selection_* otherwise
--   -- copy_mode_active_highlight_bg = { Color = '#000000' },
--   -- use `AnsiColor` to specify one of the ansi color palette values
--   -- (index 0-15) using one of the names "Black", "Maroon", "Green",
--   --  "Olive", "Navy", "Purple", "Teal", "Silver", "Grey", "Red", "Lime",
--   -- "Yellow", "Blue", "Fuchsia", "Aqua" or "White".
--   -- copy_mode_active_highlight_fg = { AnsiColor = 'Black' },
--   -- copy_mode_inactive_highlight_bg = { Color = '#52ad70' },
--   -- copy_mode_inactive_highlight_fg = { AnsiColor = 'White' },

--   -- quick_select_label_bg = { Color = 'peru' },
--   -- quick_select_label_fg = { Color = '#ffffff' },
--   -- quick_select_match_bg = { AnsiColor = 'Navy' },
--   -- quick_select_match_fg = { Color = '#ffffff' },

--   tab_bar = {
--     -- The color of the strip that goes along the top of the window
--     -- (does not apply when fancy tab bar is in use)
--     background = '#21242b',

--     -- The active tab is the one that has focus in the window
--     active_tab = {
--       -- The color of the background area for the tab
--       bg_color = '#21242b',
--       -- The color of the text for the tab
--       fg_color = '#bbc2cf',

--       -- Specify whether you want "Half", "Normal" or "Bold" intensity for the
--       -- label shown for this tab.
--       -- The default is "Normal"
--       intensity = 'Normal',

--       -- Specify whether you want "None", "Single" or "Double" underline for
--       -- label shown for this tab.
--       -- The default is "None"
--       underline = 'None',

--       -- Specify whether you want the text to be italic (true) or not (false)
--       -- for this tab.  The default is false.
--       italic = false,

--       -- Specify whether you want the text to be rendered with strikethrough (true)
--       -- or not for this tab.  The default is false.
--       strikethrough = false,
--     },

--     -- Inactive tabs are the tabs that do not have focus
--     inactive_tab = {
--       bg_color = '#282c34',
--       fg_color = '#5b6268',

--       -- The same options that were listed under the `active_tab` section above
--       -- can also be used for `inactive_tab`.
--     },

--     -- You can configure some alternate styling when the mouse pointer
--     -- moves over inactive tabs
--     inactive_tab_hover = {
--       bg_color = '#3b3052',
--       fg_color = '#909090',
--       italic = true,

--       -- The same options that were listed under the `active_tab` section above
--       -- can also be used for `inactive_tab_hover`.
--     },

--     -- The new tab button that let you create new tabs
--     new_tab = {
--       bg_color = '#282c34',
--       fg_color = '#808080',

--       -- The same options that were listed under the `active_tab` section above
--       -- can also be used for `new_tab`.
--     },

--     -- You can configure some alternate styling when the mouse pointer
--     -- moves over the new tab button
--     new_tab_hover = {
--       bg_color = '#3b3052',
--       fg_color = '#909090',
--       italic = true,

--       -- The same options that were listed under the `active_tab` section above
--       -- can also be used for `new_tab_hover`.
--     },
--   },
-- }


return config
