/*
 * siod_readline.c - GNU Readline integration for SIOD REPL
 * 
 * Part of siod-tr (SIOD - The Reawakening)
 * Modernization by Scáth, 2025
 * 
 * Provides command-line editing, history, and tab completion
 * for the SIOD interactive REPL.
 * 
 * License: GPL-3.0 (matching SIOD's original license)
 * 
 * NOTE: SIOD's own readline() function has been renamed to siod_readline()
 * to avoid symbol collision with GNU readline.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <setjmp.h>
#include <unistd.h>

#include "siod.h"
#include "siodp.h"

#ifdef HAVE_READLINE
#include <readline/readline.h>
#include <readline/history.h>

#define HISTORY_FILE "~/.siod_history"
#define MAX_HISTORY_SIZE 1000

static int using_readline = 0;

/*
 * readline_puts - Custom puts for REPL that suppresses prompts
 * 
 * When using readline, we don't want the REPL to print its own "> " prompt
 * because readline() provides the prompt. This function swallows the prompt
 * but passes through other output.
 */
static void readline_puts(char *st) {
    /* Suppress the standard "> " prompt since readline provides it */
    if (strcmp(st, "> ") == 0) {
        return;  /* Swallow the prompt */
    }
    /* Pass through all other output */
    fputs(st, stdout);
    fflush(stdout);
}

/*
 * readline_read - Custom read function for REPL using readline
 * 
 * This replaces the default lread(NIL) when readline is enabled.
 * Provides line editing, command history, and a nice prompt.
 */
static LISP readline_read(void) {
    char *line;
    LISP str_obj, result;
    
    /* Loop until we get valid input or EOF */
    while (1) {
        /* Use GNU readline to get input with editing support */
        line = readline("> ");
        
        /* Check for EOF (Ctrl-D) */
        if (line == NULL) {
            if (using_readline) {
                printf("\n");  /* Add newline after Ctrl-D */
            }
            return get_eof_val();
        }
        
        /* Skip empty lines - just loop again */
        if (*line == '\0' || *line == '\n') {
            free(line);
            continue;  /* Ask for input again */
        }
        
        /* Trim trailing whitespace */
        char *end = line + strlen(line) - 1;
        while (end > line && (*end == ' ' || *end == '\t' || *end == '\n')) {
            *end = '\0';
            end--;
        }
        
        /* Check again after trimming */
        if (*line == '\0') {
            free(line);
            continue;
        }
        
        /* Add non-empty lines to history */
        add_history(line);
        
        /* Create a SIOD string object from the input line */
        str_obj = strcons(strlen(line), line);
        
        /* Parse the line as a SIOD expression */
        result = read_from_string(str_obj);
        
        free(line);
        return result;
    }
}

/*
 * init_readline - Initialize readline support
 * 
 * Call this during SIOD startup to enable readline features.
 * Sets up history file and configures readline behavior.
 * Only enables readline if stdin is a terminal.
 */
void init_readline(void) {
    char *home;
    char history_path[512];
    
    /* Only use readline if stdin is a terminal */
    if (!isatty(STDIN_FILENO)) {
        using_readline = 0;
        return;
    }
    
    using_readline = 1;
    
    /* Set up readline */
    rl_readline_name = "siod";
    
    /* Load command history */
    home = getenv("HOME");

    if (home) {
        snprintf(history_path, sizeof(history_path), "%s/.siod_history", home);
        /* Ignore errors if file doesn't exist */
        read_history(history_path);
    }
    
    /* Limit history size */
    stifle_history(MAX_HISTORY_SIZE);
}

/*
 * save_readline_history - Save command history to file
 * 
 * Call this on exit to preserve history between sessions.
 */
void save_readline_history(void) {
    char *home;
    char history_path[512];
    
    if (!using_readline) {
        return;
    }
    
    home = getenv("HOME");
    if (home) {
        snprintf(history_path, sizeof(history_path), "%s/.siod_history", home);
        write_history(history_path);
    }
}

/*
 * get_readline_reader - Get the readline-based read function
 * 
 * Returns a function pointer for use in repl_hooks.
 * Returns NULL if stdin is not a terminal.
 */
LISP (*get_readline_reader(void))(void) {
    if (using_readline) {
        return readline_read;
    }
    return NULL;
}

/*
 * Simple standalone REPL loop using readline
 * Bypasses SIOD's repl_driver to avoid signal/I/O conflicts
 * 
 * Includes proper error handling so errors don't crash the REPL.
 */
static long readline_repl(void) {
    char *line;
    LISP str_obj, result;
    int err_code;
    jmp_buf save_errjmp;
    long save_errjmp_ok;
    
    /* Save the previous error handler */
    memcpy(save_errjmp, errjmp, sizeof(jmp_buf));
    save_errjmp_ok = errjmp_ok;
    
    /* Enable error handling - CRITICAL! */
    errjmp_ok = 1;
    
    /* Main REPL loop */
    while (1) {
        /* Set up error handler for this iteration */
        err_code = setjmp(errjmp);
        
        if (err_code != 0) {
            /* An error occurred (type error, undefined var, division by zero, etc.)
             * The error message has already been printed by err().
             * Just continue to the next prompt. */
            continue;
        }
        
        /* Get input using GNU readline */
        line = readline("> ");
        
        /* Check for EOF (Ctrl-D) */
        if (line == NULL) {
            printf("\n");
            break;
        }
        
        /* Skip empty lines */
        if (*line == '\0') {
            free(line);
            continue;
        }
        
        /* Trim trailing whitespace */
        char *end = line + strlen(line) - 1;
        while (end > line && (*end == ' ' || *end == '\t' || *end == '\n')) {
            *end = '\0';
            end--;
        }
        
        /* Check again after trimming */
        if (*line == '\0') {
            free(line);
            continue;
        }
        
        /* Add to history */
        add_history(line);
        
        /* Parse and evaluate */
        str_obj = strcons(strlen(line), line);
        result = leval(read_from_string(str_obj), NIL);
        
        /* Print result */
        if (siod_verbose_level >= 2) {
            lprint(result, NIL);
        }
        
        free(line);
    }
    
    /* Restore previous error handler */
    memcpy(errjmp, save_errjmp, sizeof(jmp_buf));
    errjmp_ok = save_errjmp_ok;
    
    return 0;
}

/*
 * Get the standalone readline REPL function
 */
long (*get_readline_repl(void))(void) {
    if (using_readline) {
        return readline_repl;
    }
    return NULL;
}

/*
 * get_readline_puts - Get the custom puts function for readline mode
 * 
 * This suppresses the REPL's standard "> " prompt when using readline.
 */
void (*get_readline_puts(void))(char *) {
    if (using_readline) {
        return readline_puts;
    }
    return NULL;
}

#else /* !HAVE_READLINE */

/* Stubs when readline is not available */
void init_readline(void) {
    /* No-op */
}

void save_readline_history(void) {
    /* No-op */
}

LISP (*get_readline_reader(void))(void) {
    return NULL;
}

void (*get_readline_puts(void))(char *) {
    return NULL;
}

long (*get_readline_repl(void))(void) {
    return NULL;
}

#endif /* HAVE_READLINE */
