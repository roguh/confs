w = 300

conky.config = {
    alignment = 'top_right',
    background = true,
    border_width = 0,
    cpu_avg_samples = 10,
	default_color = xrdb('foreground'),
    default_outline_color = 'white',
    default_shade_color = 'black',
    double_buffer = true,
    draw_borders = false,
    draw_graph_borders = false,
    draw_outline = false,
    draw_shades = false,
    use_xft = true,
    font = 'Noto Sans:normal:size=15',
    border_inner_margin = w / 3, -- TODO ????
    gap_x = - 1.1 * w,
    gap_y = 0.1 * w,
    net_avg_samples = 2,
    out_to_console = false,
    out_to_stderr = false,
    extra_newline = false,
    own_window = true,
    own_window_class = 'Conky',
    own_window_type = 'override',
    own_window_transparent = true,
    update_interval = 1.0,
    
    minimum_width = w,
}

conky.text = interp([[
#{main}${time %H:%M:%S}#{normal}

${tztime Asia/Shanghai #{t}} $alignr #{note}Shenzhen
#{normal}${tztime Asia/Tokyo #{t}} $alignr #{note}Tokyo
#{normal}${tztime Pacific/Honolulu #{t}} $alignr #{note}Honolulu

#{normal}${tztime America/Los_Angeles #{t}} $alignr #{note}LA
#{normal}${tztime America/Denver #{t}} $alignr #{note}NM
#{normal}${tztime America/Mexico_City #{t}} $alignr #{note}Cd. MX
#{normal}${tztime America/New_York #{t}} $alignr #{note}NY

#{normal}${tztime Europe/Amsterdam #{t}} $alignr #{note}Amsterdam

]], {
    normal = '$font',
    main = '${font Noto Sans:thin:size=50}',
    sub = '${font Noto Sans:thin:size=20}',
    note = '${font Noto Sans:thin:size=15}',
    t = '%H:%M %p %Z'
})
