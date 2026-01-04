local M = {}

-- Store the job handle for the Go binary
local job_id = nil
local buf_id = nil
local win_id = nil

-- Path to the Go binary (will be built later)
local binary_path = vim.fn.stdpath('data') .. '/bubbletea-tui/bubbletea-tui'

-- Function to build the Go binary if it doesn't exist
local function ensure_binary()
    if vim.fn.executable(binary_path) == 0 then
        -- Try to build the binary
        local plugin_dir = vim.fn.fnamemodify(debug.getinfo(1).source:sub(2), ':h:h:h')
        local build_cmd = 'cd ' .. plugin_dir .. ' && go build -o ' .. binary_path .. ' ./cmd/bubbletea-tui'
        
        -- Create the directory for the binary
        vim.fn.mkdir(vim.fn.fnamemodify(binary_path, ':h'), 'p')
        
        -- Build the binary
        local result = vim.fn.system(build_cmd)
        if vim.v.shell_error ~= 0 then
            vim.api.nvim_err_writeln('Failed to build bubbletea-tui binary: ' .. result)
            return false
        end
    end
    return true
end

-- Function to start the Bubble Tea demo
function M.start_demo()
    if not ensure_binary() then
        return
    end
    
    -- Stop any existing instance
    M.stop()
    
    -- Create a new buffer for the TUI
    buf_id = vim.api.nvim_create_buf(false, true)
    
    -- Set buffer options
    vim.api.nvim_buf_set_option(buf_id, 'buftype', 'nofile')
    vim.api.nvim_buf_set_option(buf_id, 'swapfile', false)
    vim.api.nvim_buf_set_option(buf_id, 'bufhidden', 'wipe')
    
    -- Open the buffer in a new window
    vim.cmd('split')
    win_id = vim.api.nvim_get_current_win()
    vim.api.nvim_win_set_buf(win_id, buf_id)
    
    -- Set window options for better TUI display
    vim.api.nvim_win_set_option(win_id, 'number', false)
    vim.api.nvim_win_set_option(win_id, 'relativenumber', false)
    vim.api.nvim_win_set_option(win_id, 'signcolumn', 'no')
    vim.api.nvim_win_set_option(win_id, 'wrap', false)
    
    -- Get window dimensions
    local width = vim.api.nvim_win_get_width(win_id)
    local height = vim.api.nvim_win_get_height(win_id)
    
    -- Start the Go binary as a terminal job
    job_id = vim.fn.termopen(binary_path .. ' --width=' .. width .. ' --height=' .. height, {
        on_exit = function(job_id, exit_code, event_type)
            if buf_id and vim.api.nvim_buf_is_valid(buf_id) then
                vim.api.nvim_buf_delete(buf_id, {force = true})
            end
            job_id = nil
            buf_id = nil
            win_id = nil
        end
    })
    
    if job_id == -1 then
        vim.api.nvim_err_writeln('Failed to start bubbletea-tui')
        return
    end
    
    -- Enter terminal mode
    vim.cmd('startinsert')
    
    print('Bubble Tea TUI started! Press <Esc> to exit terminal mode, :BubbleTeaStop to stop.')
end

-- Function to stop the Bubble Tea TUI
function M.stop()
    if job_id then
        vim.fn.jobstop(job_id)
        job_id = nil
    end
    
    if buf_id and vim.api.nvim_buf_is_valid(buf_id) then
        vim.api.nvim_buf_delete(buf_id, {force = true})
        buf_id = nil
    end
    
    win_id = nil
    print('Bubble Tea TUI stopped.')
end

return M

