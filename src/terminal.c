/* ***************************************************************************
--          Forth Terminal control helper forinterpreter 
--
--           Copyright (C) 2026 By Ulrik Hørlyk Hjort
--
-- Permission is hereby granted, free of charge, to any person obtaining
-- a copy of this software and associated documentation files (the
-- "Software"), to deal in the Software without restriction, including
-- without limitation the rights to use, copy, modify, merge, publish,
-- distribute, sublicense, and/or sell copies of the Software, and to
-- permit persons to whom the Software is furnished to do so, subject to
-- the following conditions:
--
-- The above copyright notice and this permission notice shall be
-- included in all copies or substantial portions of the Software.
--
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
-- EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
-- MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
-- NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
-- LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
-- OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
-- WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
-- ***************************************************************************/


#include <termios.h>
#include <unistd.h>

static struct termios original_termios;
static int terminal_saved = 0;

/* Set terminal to raw mode for line editing */
int forth_terminal_set_raw(void) {
    struct termios raw;
    
    if (!terminal_saved) {
        if (tcgetattr(STDIN_FILENO, &original_termios) < 0)
            return -1;
        terminal_saved = 1;
    }
    
    raw = original_termios;
    
    /* Disable canonical mode and echo */
    raw.c_lflag &= ~(ECHO | ICANON);
    
    /* Keep output processing for proper newlines */
    raw.c_oflag |= (OPOST | ONLCR);
    
    /* Non-blocking read: return immediately */
    raw.c_cc[VMIN] = 0;
    raw.c_cc[VTIME] = 0;
    
    return tcsetattr(STDIN_FILENO, TCSANOW, &raw);
}

/* Restore original terminal settings */
int forth_terminal_restore(void) {
    if (!terminal_saved)
        return 0;
    return tcsetattr(STDIN_FILENO, TCSANOW, &original_termios);
}
