-- TODO: run :x/ *$/ c// if this would leave the original file unchanged

-- load standard vis module, providing parts of the Lua API
require('vis')
require('plugins/filetype')
require('plugins/textobject-lexer')
require('local-plugins/vis-cursors/cursors')
backup = require('local-plugins/vis-backup/backup')

vis.events.subscribe(vis.events.INIT, function()
	-- Global configuration options.

	backup.time_format = "%H-%M-%S-"
	backup.directory = os.getenv("HOME") .. "/tmp/backup" 
	backup.get_fname = backup.entire_path_with_timestamp
end)

vis.events.subscribe(vis.events.WIN_OPEN, function(win)
	-- Per window configuration options.
	vis:command('set relativenumber')
	vis:command('set colorcolumn 79')
	vis:command('set tabwidth 2')
	vis:command('set expandtab on')
	vis:command('set autoindent')
	-- vis:command('set cursorline on')
	vis:command('set theme base16-onedark')
end)
