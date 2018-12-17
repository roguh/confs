-- TODO: run :x/ *$/ c// if this would leave the original file unchanged

-- load standard vis module, providing parts of the Lua API
require('vis')
require('plugins/filetype')
require('plugins/textobject-lexer')

function file_exists(name)
   local f=io.open(name,"r")
   if f~=nil then io.close(f) return true else return false end
end

function require_if_exists(name)
  if file_exists(name) then
    require(name)
  end
end

require_if_exists('local-plugins/vis-cursors/cursors')
require_if_exists('local-plugins/vis-modelines/vis-modelines')

-- require('local-plugins/vis-editorconfig/editorconfig')
-- require('local-plugins/vis-whitespace-cleanup/whitespace')
backup = require_if_exists('local-plugins/vis-backup/backup') or {}

vis.events.subscribe(vis.events.INIT, function()
	-- Global configuration options.

	backup.time_format = "%H-%M-%S"
	backup.directory = os.getenv("HOME") .. "/tmp/backup"
	backup.get_fname = backup.entire_path_with_timestamp
	-- vis:command('set theme base16-unikitty-dark')
	-- vis:command('set theme vistheme')
    vis.lexers.STYLE_CURSOR = 'reverse'
    vis.lexers.STYLE_CURSOR_LINE = 'bold,underlined'
    vis.lexers.STYLE_COLOR_COLUMN = 'reverse'
end)

vis.events.subscribe(vis.events.WIN_OPEN, function(win)
	-- Per window configuration options.
	vis:command('set number')
	vis:command('set relativenumber')
	vis:command('set colorcolumn 79')
	vis:command('set tabwidth 4')
	vis:command('set expandtab on')
	vis:command('set autoindent')
	vis:command('set cursorline on')
	vis:command('set show-tabs on')
	vis:command('set show-newlines on')
	if vis.option_get then
	vis:info('option: ' .. tostring(vis:option_get("lol")))
	end
end)
