-- Hugo O. Rivera's vis configuration
-- Hugo O. Rivera's vis configuration

-- TODO ranger file browser or alternative
-- TODO simple version of https://github.com/rhysd/committia.vim/tree/master/autoload ?
-- TODO rainbow indentation?
-- TODO org-mode hierarchy support? navigate to next level, create new level, etc
-- TODO run code checks and githooks automatically or with hotkey? format on save?
-- TODO autoreload on changes??? (IMPORTANT)


-- Load standard vis module, providing parts of the Lua API
require('vis')

vis.events.subscribe(vis.events.INIT, function(win)
    -- Semicolon is the same as colon
    vis:map(vis.modes.NORMAL, ';', ':')
    
    -- Alias shell redirection commands
    -- These won't work in VISUAL since they're taken by other mappings
    -- TODO force a mapping?
    vis:map(vis.modes.NORMAL | vis.modes.VISUAL_LINE, '|', ':|')
    vis:map(vis.modes.NORMAL | vis.modes.VISUAL_LINE, '<', ':<')
    vis:map(vis.modes.NORMAL | vis.modes.VISUAL_LINE, '>', ':>')
    
    vis:map(vis.modes.NORMAL, '`', ':!makeanywhere format || makeanywhere format-fix')
    
    -- The famous ctrl-P for finding files
    vis:map(vis.modes.NORMAL | vis.modes.VISUAL_LINE | vis.modes.VISUAL,
        '<C-p>', function() vis:command(':fzf') end)
    vis:map(vis.modes.NORMAL | vis.modes.VISUAL_LINE | vis.modes.VISUAL,
        '<C-f>', function() vis:command(':fzf') end)
    
    -- Paste from system clipboard with vis-clipboard
    vis:map(vis.modes.NORMAL, '<C-v>', '"+p')
    vis:map(vis.modes.INSERT, '<C-v>', '<Escape>"+pi')
    
    -- Put selection content into system clipboard
    vis:map(vis.modes.VISUAL_LINE, '<C-c>', function() vis:feedkeys(':>vis-clipboard --copy<Enter>') end)
    -- TODO this doesn't work with multiple cursors, just gets last selection
    vis:map(vis.modes.VISUAL, '<C-c>', function() vis:feedkeys(':>vis-clipboard --copy<Enter>') end)
end)

vis.events.subscribe(vis.events.WIN_OPEN, function(win)
    -- Per window configuration options.
    vis:command('set shell sh') -- a barebones shell that should have no extra config/outputs
    vis:command('set number')
    vis:command('set relativenumber')
    vis:command('set autoindent')
    vis:command('set cursorline on')

    -- Python formatting settings
    vis:command('set colorcolumn 79')
    vis:command('set tabwidth 4')
    vis:command('set expandtab on')
    if vis.option_get then
      vis:info('option: ' .. tostring(vis:option_get("lol")))
    end
end)

vis.events.subscribe(vis.events.INIT, function()
    -- Load plugins with require(), download if necessary
    lint = require_if_exists('https://github.com/roguh/vis-lint', 'vis-lint') or {}
    backup = require_if_exists('https://github.com/roguh/vis-backup', 'vis-backup', 'backup') or {}
    shebang = require_if_exists('https://github.com/e-zk/vis-shebang', 'vis-shebang') or {}

    -- A global variable for configuring vis-shebang
    shebangs = {
        ["#!/bin/sh"] = "bash",
        ["#!/bin/bash"] = "bash",
        ["#!/usr/bin/env python3"] = "python",
        ["#!/usr/bin/env python"] = "python",
    }

    -- Configure backup plugin
    backup.time_format = "%H-%M-%S"
    -- This will create the directory
    backup.set_directory(os.getenv("HOME") .. "/tmp/vis-backups")
    backup.get_fname = backup.entire_path_with_double_percentage_signs_and_timestamp
end)

function file_exists(name)
   local fileobj = io.open(name, "r")
   if fileobj ~= nil
   then io.close(fileobj) return true
   else return false
   end
end

-- Copied with love from
-- https://git.sr.ht/~mcepl/vis-fzf-open/tree/master/item/init.lua
fzf_config = {}
fzf_config.fzf_path = "fzf"
fzf_config.fzf_args = ""

vis:command_register("fzf", function(argv, force, win, selection, range)
    local command = string.gsub([[
            $fzf_path \
                --header="Enter:edit,^s:split,^v:vsplit" \
                --expect="ctrl-s,ctrl-v" \
                $fzf_args $args
        ]],
        '%$([%w_]+)', {
            fzf_path=fzf_config.fzf_path,
            fzf_args=fzf_config.fzf_args,
            args=table.concat(argv, " ")
        }
    )

    local file = io.popen(command)
    local output = {}
    for line in file:lines() do
        table.insert(output, line)
    end
    local success, msg, status = file:close()

    if status == 0 then
        local action = 'e'

        if     output[1] == 'ctrl-s' then action = 'split'
        elseif output[1] == 'ctrl-v' then action = 'vsplit'
        end

        vis:feedkeys(string.format(":%s '%s'<Enter>", action, output[2]))
    elseif status == 1 then
        vis:info(
            string.format(
                "fzf-open: No match. Command %s exited with return value %i.",
                command, status
            )
        )
    elseif status == 2 then
        vis:info(
            string.format(
                "fzf-open: Error. Command %s exited with return value %i.",
                command, status
            )
        )
    elseif status == 130 then
        vis:info(
            string.format(
                "fzf-open: Interrupted. Command %s exited with return value %i",
                command, status
            )
        )
    else
        vis:info(
            string.format(
                "fzf-open: Unknown exit status %i. command %s exited with return value %i",
                status, command, status
            )
        )
    end

    vis:feedkeys("<vis-redraw>")

    return true;
end, "Select file to open with fzf")

function run_command(command)
    return io.popen(command):read('*a'):gsub('\n', '')
end

-- Based on https://github.com/erf/vis-title/blob/master/init.luoa
function set_title(fname)
    local full_title = 'vis ' .. fname .. ' ' .. run_command('hostname')
    vis:command(string.format(":!printf '\\033]0;%s\\007'", full_title))
end

vis.events.subscribe(vis.events.WIN_OPEN, function(win)
    set_title(win.file.name or '[No Name]')
end)

vis.events.subscribe(vis.events.FILE_SAVE_POST, function(file, path)
    set_title(file.name)
end)

-- For loading plugins
function require_if_exists(repo, directory, initfile)
  initfile = initfile or 'init'
  local name = directory .. '/' .. initfile
  local location = os.getenv("HOME") .. "/.config/vis/"
  local plugin_location = location .. directory

  if os.rename(plugin_location, plugin_location) == nil then
    run_command('cd ' .. location .. ' && git clone ' .. repo .. ' ' .. plugin_location)
    vis:message('Installed plugin from repo: ' .. repo .. ' in ' .. location)
  end

  return require(name)
end

-- PURTT COLORS
purty_colors_now = function(a, b, c, d)
    local light_mode = false
    if light_mode then
        vis:command('set theme mellow')
        return null
    end

    local lexers = vis.lexers
    
    -- Basic Style
    lexers.STYLE_CURSOR = 'reverse'
    lexers.STYLE_CURSOR_LINE = 'bold,underlined'
    lexers.STYLE_COLOR_COLUMN = 'reverse'

    -- Copied from:
    -- base16-vis (https://github.com/pshevtsov/base16-vis)
    -- by Petr Shevtsov
    -- Grayscale Dark scheme by Alexandre Gavioli (https://github.com/Alexx2/)
    local colors = {
        ['base00'] = '#101010',
        ['base01'] = '#252525',
        ['base02'] = '#464646',
        ['base03'] = '#ff1694',
        ['base04'] = '#ababab',
        ['base05'] = '#b9b9b9',
        ['base06'] = '#e3e3e3',
        ['base07'] = '#f7f7f7',
        ['base08'] = '#7c7c7c',
        ['base09'] = '#999999',
        ['base0A'] = '#a0a0a0',
        ['base0B'] = '#8e8e8e',
        ['base0C'] = '#868686',
        ['base0D'] = '#686868',
        ['base0E'] = '#747474',
        ['base0F'] = '#5e5e5e',
    }
    
    lexers.colors = colors
    
    local fg = ',fore:'..colors.base05..','
    local bg = ',back:'..colors.base00..','
    
    lexers.STYLE_DEFAULT = bg..fg
    lexers.STYLE_NOTHING = bg
    lexers.STYLE_CLASS = 'fore:'..colors.base0A
    lexers.STYLE_COMMENT = 'fore:'..colors.base03..',italics'
    lexers.STYLE_CONSTANT = 'fore:'..colors.base09
    lexers.STYLE_DEFINITION = 'fore:'..colors.base0E
    lexers.STYLE_ERROR = 'fore:'..colors.base03..',italics'
    lexers.STYLE_FUNCTION = 'fore:'..colors.base0D
    lexers.STYLE_KEYWORD = 'fore:'..colors.base0E
    lexers.STYLE_LABEL = 'fore:'..colors.base0A
    lexers.STYLE_NUMBER = 'fore:'..colors.base09
    lexers.STYLE_OPERATOR = 'fore:'..colors.base05
    lexers.STYLE_REGEX = 'fore:'..colors.base0C
    lexers.STYLE_STRING = 'fore:'..'#ff1616'
    lexers.STYLE_PREPROCESSOR = 'fore:'..colors.base0A
    lexers.STYLE_TAG = 'fore:'..colors.base0A
    lexers.STYLE_TYPE = 'fore:'..colors.base0A
    lexers.STYLE_VARIABLE = 'fore:'..colors.base0D
    lexers.STYLE_WHITESPACE = 'fore:'..colors.base02
    lexers.STYLE_EMBEDDED = 'fore:'..colors.base0F
    lexers.STYLE_IDENTIFIER = 'fore:'..colors.base08
    
    lexers.STYLE_LINENUMBER = 'fore:'..colors.base02..',back:'..colors.base00
    lexers.STYLE_CURSOR = 'fore:'..colors.base00..',back:'..colors.base05
    lexers.STYLE_CURSOR_PRIMARY = 'fore:'..colors.base00..',back:'..colors.base05
    lexers.STYLE_CURSOR_LINE = 'back:'..colors.base01
    lexers.STYLE_COLOR_COLUMN = 'back:'..colors.base01
    lexers.STYLE_SELECTION = 'back:'..colors.base02
    lexers.STYLE_STATUS = 'fore:'..colors.base04..',back:'..colors.base01
    lexers.STYLE_STATUS_FOCUSED = 'fore:'..colors.base09..',back:'..colors.base01
    lexers.STYLE_SEPARATOR = lexers.STYLE_DEFAULT
    lexers.STYLE_INDENTGUIDE = 'fore:white'
    lexers.STYLE_INFO = 'fore:default,back:default,bold'
    lexers.STYLE_EOF = ''
end

vis.events.subscribe(vis.events.INIT, purty_colors_now)
vis.events.subscribe(vis.events.WIN_OPEN, purty_colors_now)

-- https://gitlab.com/muhq/vis-lspc ? Might slow vis down too much

-- Reminders:
-- :x/.../ c/.../        is   %s/.../.../ (g?)
-- It even leaves you multiple cursors at each replacement for further refinement!

-- :r                    is   :read
-- No, I didn't figure out how to alias it

-- :!command %           is   :<command $vis_filename
-- Why vis, why. TODO alias somehow? fork vis?

-- :sort is now :|sort
-- Or use my alias:    :| is |   in most modes

-- Move around windows with Ctrl-W hjkl, no arrow keys
-- TODO alias arrow keys for switching windows

-- > and < in VISUAL modify indentation

-- Ctrl-n C-n is TAB for autocomplete
