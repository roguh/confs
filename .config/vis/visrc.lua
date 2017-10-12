-- load standard vis module, providing parts of the Lua API
require('vis')
require('plugins/filetype')
require('plugins/textobject-lexer')

vis.events.subscribe(vis.events.INIT, function()
	-- Global configuration options.
end)

vis.events.subscribe(vis.events.WIN_OPEN, function(win)
	-- Per window configuration options.
	vis:command('set relativenumber')
	vis:command('set cursorline on')
	vis:command('set colorcolumn 79')
	vis:command('set tabwidth 2')
	vis:command('set expandtab on')
	vis:command('set autoindent')
end)
