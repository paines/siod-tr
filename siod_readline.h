/*
 * siod_readline.h - Readline module header for SIOD
 * 
 * Part of siod-tr (SIOD - The Reawakening)
 * Modernization by Scáth, 2025
 * 
 * License: GPL-3.0
 * 
 * NOTE: siod.h and siodp.h must be included before this header
 */

#ifndef SIOD_READLINE_H
#define SIOD_READLINE_H

/* Initialize readline support */
void init_readline(void);

/* Save history on exit */
void save_readline_history(void);

/* Get the readline-based read function for REPL hooks */
LISP (*get_readline_reader(void))(void);

/* Get the custom puts function that suppresses prompts */
void (*get_readline_puts(void))(char *);

/* Get the standalone readline REPL function (bypasses repl_driver) */
long (*get_readline_repl(void))(void);

#endif /* SIOD_READLINE_H */
