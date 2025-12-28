/*
 * sql_sqlite3.c - SQLite3 bindings for SIOD
 *
 * (c) DeeDeeCee, 2025-12-23
 * 
 * Compile with gcc -c -DUSE_DL=1 sql_sqlite3.c
 * Link withL -lsqlite3
 *
 * Enjoy the pointlessness!
 *
 */


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sqlite3.h>
#include "siod.h"

/* Type tag for SQLite3 database handles */
static long tc_sqlite_db = 0;

/* Type tag for SQLite3 statement handles */
static long tc_sqlite_stmt = 0;

/* Wrapper structures */
typedef struct {
    sqlite3 *db;
    int open;
} db_handle;

typedef struct {
    sqlite3_stmt *stmt;
    int active;
} stmt_handle;

/* Forward declarations */
static void init_sqlite3_types(void);
static LISP lsqlite3_open(LISP filename);
static LISP lsqlite3_close(LISP db_obj);
static LISP lsqlite3_exec(LISP db_obj, LISP sql);
static LISP lsqlite3_prepare(LISP db_obj, LISP sql);
static LISP lsqlite3_step(LISP stmt_obj);
static LISP lsqlite3_finalize(LISP stmt_obj);
static LISP lsqlite3_bind(LISP stmt_obj, LISP index, LISP value);
static LISP lsqlite3_column(LISP stmt_obj, LISP index);
static LISP lsqlite3_column_count(LISP stmt_obj);
static LISP lsqlite3_reset(LISP stmt_obj);
static LISP lsqlite3_errcode(LISP db_obj);
static LISP lsqlite3_errmsg(LISP db_obj);


/* Print function for db handles */
static void sqlite_db_prin1(LISP ptr, struct gen_printio *f) {

  char buff[256];
  sqlite3 *db = (sqlite3 *)ptr->storage_as.string.data;
  snprintf(buff, sizeof(buff), "#<SQLITE-DB %p>", (void*)db);
    f->puts_fcn(buff, f->cb_argument);
}

/* Print function for stmt handles */
static void sqlite_stmt_prin1(LISP ptr, struct gen_printio *f) {
  char buff[256];
  sqlite3_stmt *stmt = (sqlite3_stmt *)ptr->storage_as.string.data;
  snprintf(buff, sizeof(buff), "#<SQLITE-STMT %p>", (void*)stmt);
  f->puts_fcn(buff, f->cb_argument);
}

/* Garbage collection marker - nothing to mark for these types */
static void sqlite_db_gc_mark(LISP ptr) {
}

static void sqlite_stmt_gc_mark(LISP ptr) {
}
	

/* Finalisers */
static void sqlite_db_gc_free(LISP ptr) {
    db_handle *dbh = (db_handle *)ptr->storage_as.string.data;
    if (dbh && dbh->open && dbh->db) {
        sqlite3_close(dbh->db);
        dbh->open = 0;
    }
}

static void sqlite_stmt_gc_free(LISP ptr) {
    stmt_handle *sh = (stmt_handle *)ptr->storage_as.string.data;
    if (sh && sh->active && sh->stmt) {
        sqlite3_finalize(sh->stmt);
        sh->active = 0;
    }
}

/* Initialize type tags */
static void init_sqlite3_types(void) {
    static long db_kind = 0;
    static long stmt_kind = 0;

    tc_sqlite_db = allocate_user_tc();
    set_gc_hooks(tc_sqlite_db,
                 NULL, 
                 sqlite_db_gc_mark,
                 NULL, 
                 sqlite_db_gc_free,
                 &db_kind ); 

    tc_sqlite_stmt = allocate_user_tc();
    set_gc_hooks(tc_sqlite_stmt,
                 NULL,
                 sqlite_stmt_gc_mark,
                 NULL,
                 sqlite_stmt_gc_free,
                 &stmt_kind);

    set_print_hooks(tc_sqlite_db, sqlite_db_prin1);
    set_print_hooks(tc_sqlite_stmt, sqlite_stmt_prin1);
}

/* Helper: create db handle object */
static LISP make_db_handle(sqlite3 *db) {
    LISP obj;
    db_handle *dbh = (db_handle *)malloc(sizeof(db_handle));
    dbh->db = db;
    dbh->open = 1;

    obj = cons(NIL, NIL);
    obj->type = tc_sqlite_db;
    obj->storage_as.string.data = (char *)dbh;
    obj->storage_as.string.dim = sizeof(db_handle);

    return obj;
}

/* Helper: create stmt handle object */
static LISP make_stmt_handle(sqlite3_stmt *stmt) {
    LISP obj;
    stmt_handle *sh = (stmt_handle *)malloc(sizeof(stmt_handle));
    sh->stmt = stmt;
    sh->active = 1;

    obj = cons(NIL, NIL);
    obj->type = tc_sqlite_stmt;
    obj->storage_as.string.data = (char *)sh;
    obj->storage_as.string.dim = sizeof(stmt_handle);

    return obj;
}

/* Helper: extract db handle */
static db_handle *get_db_handle(LISP obj) {
    if (TYPEP(obj, tc_sqlite_db)) {
        return (db_handle *)obj->storage_as.string.data;
    }
    err("not a sqlite database handle", obj);
    return NULL;
}

/* Helper: extract stmt handle */
static stmt_handle *get_stmt_handle(LISP obj) {
    if (TYPEP(obj, tc_sqlite_stmt)) {
        return (stmt_handle *)obj->storage_as.string.data;
    }
    err("not a sqlite statement handle", obj);
    return NULL;
}

/* (sqlite3-open filename) -> db-handle */
static LISP lsqlite3_open(LISP filename) {
    sqlite3 *db;
    int rc;
    char *fname;

    fname = get_c_string(filename);
    rc = sqlite3_open(fname, &db);

    if (rc != SQLITE_OK) {
        const char *errmsg = sqlite3_errmsg(db);
        sqlite3_close(db);
        return err(errmsg, filename);
    }

    return make_db_handle(db);
}

/* (sqlite3-close db) -> nil */
static LISP lsqlite3_close(LISP db_obj) {
    db_handle *dbh = get_db_handle(db_obj);

    if (dbh->open && dbh->db) {
        sqlite3_close(dbh->db);
        dbh->open = 0;
    }

    return NIL;
}

/* (sqlite3-exec db sql-string) -> nil or error */
static LISP lsqlite3_exec(LISP db_obj, LISP sql) {
    db_handle *dbh = get_db_handle(db_obj);
    char *sql_str, *errmsg = NULL;
    int rc;

    if (!dbh->open)
        return err("database is closed", db_obj);

    sql_str = get_c_string(sql);
    rc = sqlite3_exec(dbh->db, sql_str, NULL, NULL, &errmsg);

    if (rc != SQLITE_OK) {
        LISP error = strcons(strlen(errmsg), errmsg);
        sqlite3_free(errmsg);
        return err("SQL error", error);
    }

    return NIL;
}

/* (sqlite3-prepare db sql-string) -> stmt-handle */
static LISP lsqlite3_prepare(LISP db_obj, LISP sql) {
    db_handle *dbh = get_db_handle(db_obj);
    char *sql_str;
    sqlite3_stmt *stmt;
    int rc;

    if (!dbh->open)
        return err("database is closed", db_obj);

    sql_str = get_c_string(sql);
    rc = sqlite3_prepare_v2(dbh->db, sql_str, -1, &stmt, NULL);

    if (rc != SQLITE_OK) {
        return err(sqlite3_errmsg(dbh->db), sql);
    }

    return make_stmt_handle(stmt);
}

/* (sqlite3-step stmt) -> 'row, 'done, or 'error */
static LISP lsqlite3_step(LISP stmt_obj) {
    stmt_handle *sh = get_stmt_handle(stmt_obj);
    int rc;

    if (!sh->active)
        return err("statement is inactive", stmt_obj);

    rc = sqlite3_step(sh->stmt);

    switch(rc) {
        case SQLITE_ROW:
            return cintern("row");
        case SQLITE_DONE:
            return cintern("done");
        default:
            return cintern("error");
    }
}

/* (sqlite3-reset stmt) -> nil */
static LISP lsqlite3_reset(LISP stmt_obj) {
    stmt_handle *sh = get_stmt_handle(stmt_obj);

    if (!sh->active)
        return err("statement is inactive", stmt_obj);

    sqlite3_reset(sh->stmt);
    return NIL;
}

/* (sqlite3-finalize stmt) -> nil */
static LISP lsqlite3_finalize(LISP stmt_obj) {
    stmt_handle *sh = get_stmt_handle(stmt_obj);

    if (sh->active && sh->stmt) {
        sqlite3_finalize(sh->stmt);
        sh->active = 0;
    }

    return NIL;
}

/* (sqlite3-bind stmt index value) -> nil */
static LISP lsqlite3_bind(LISP stmt_obj, LISP index, LISP value) {
    stmt_handle *sh = get_stmt_handle(stmt_obj);
    int idx = get_c_long(index);
    int rc;

    if (!sh->active)
        return err("statement is inactive", stmt_obj);

    if (NULLP(value)) {
       rc = sqlite3_bind_null(sh->stmt, idx);
    } else if (TYPEP(value, tc_flonum)) {
       double d = FLONM(value);
      /* Check if it's an integer value */
      if (d == (long)d) {
           rc = sqlite3_bind_int64(sh->stmt, idx, (long)d);
      } else {
           rc = sqlite3_bind_double(sh->stmt, idx, d);
      }
    } else {
       char *str = get_c_string(value);
       rc = sqlite3_bind_text(sh->stmt, idx, str, -1, SQLITE_TRANSIENT);
    }

    if (rc != SQLITE_OK) {
        return err("bind failed", value);
    }

    return NIL;
}

/* (sqlite3-column stmt index) -> value */
static LISP lsqlite3_column(LISP stmt_obj, LISP index) {
    stmt_handle *sh = get_stmt_handle(stmt_obj);
    int idx = get_c_long(index);
    int col_type;

    if (!sh->active)
        return err("statement is inactive", stmt_obj);

    col_type = sqlite3_column_type(sh->stmt, idx);

    switch(col_type) {
        case SQLITE_INTEGER:
            return flocons((double)sqlite3_column_int64(sh->stmt, idx));
        case SQLITE_FLOAT:
            return flocons(sqlite3_column_double(sh->stmt, idx));
        case SQLITE_TEXT: {
            const char *text = (const char *)sqlite3_column_text(sh->stmt, idx);
            return strcons(strlen(text), text);
        }
        case SQLITE_NULL:
            return NIL;
        default:
            return NIL;
    }
}

/* (sqlite3-column-count stmt) -> integer */
static LISP lsqlite3_column_count(LISP stmt_obj) {
    stmt_handle *sh = get_stmt_handle(stmt_obj);

    if (!sh->active)
        return err("statement is inactive", stmt_obj);

    return flocons((double)sqlite3_column_count(sh->stmt));
}

/* (sqlite3-errcode db) -> integer */
static LISP lsqlite3_errcode(LISP db_obj) {
    db_handle *dbh = get_db_handle(db_obj);

    if (!dbh->open)
        return flocons(0.0);

    return flocons((double)sqlite3_errcode(dbh->db));
}

/* (sqlite3-errmsg db) -> string */
static LISP lsqlite3_errmsg(LISP db_obj) {
    db_handle *dbh = get_db_handle(db_obj);
    const char *msg;

    if (!dbh->open)
        return strcons(16, "database closed");

    msg = sqlite3_errmsg(dbh->db);
    return strcons(strlen(msg), msg);
}

/* Initialization function - call this from SIOD */
void init_sql_sqlite3(void) {
    init_sqlite3_types();

    init_subr_2("sqlite3-open", lsqlite3_open);
    init_subr_1("sqlite3-close", lsqlite3_close);
    init_subr_2("sqlite3-exec", lsqlite3_exec);
    init_subr_2("sqlite3-prepare", lsqlite3_prepare);
    init_subr_1("sqlite3-step", lsqlite3_step);
    init_subr_1("sqlite3-reset", lsqlite3_reset);
    init_subr_1("sqlite3-finalize", lsqlite3_finalize);
    init_subr_3("sqlite3-bind", lsqlite3_bind);
    init_subr_2("sqlite3-column", lsqlite3_column);
    init_subr_1("sqlite3-column-count", lsqlite3_column_count);
    init_subr_1("sqlite3-errcode", lsqlite3_errcode);
    init_subr_1("sqlite3-errmsg", lsqlite3_errmsg);
}
