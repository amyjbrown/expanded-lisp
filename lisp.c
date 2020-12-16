/* A minimal Lisp interpreter
   Copyright 2004 Andru Luvisi

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License , or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program. If not, write to the Free Software
   Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <ctype.h>

/** Error Return Macro**/

#define error(X)                \
  do                            \
  {                             \
    fprintf(stderr, "%s\n", X); \
    exit(1);                    \
  } while (0)
/*** TYPES ***/

/*** List Structured Memory ***/
enum ObjType
{
  INT,
  SYM,
  CONS,
  PROC,
  PRIMOP
};
/**
 * Object
 * This is a weird one -- it works by being a tagged union and then it has a pointer to a pointer 
 * This acts as in array pointer structure that will follow the Object when allocated in memory
 * That is, with the list (a b c d), you'd get
 * [  a  |  &b  |  &c  |  &d  ]
 * 0     O+0P   O+P    O+2P   O+3P
 * 
 * Equivalent to
 * struct Object{ enum ObjType type; Object* p[];}, or more logically char[] or void*
*/
typedef struct Object
{
  enum ObjType type;
  struct Object* p[1]; // Destructures to p1: &&Object, p1: [&OBject], or p1: [[Object]] 
} Object;


typedef Object *(*primop)(Object *);
/*** Global Constants***/
Object *all_symbols,  // Global symbol table
      *top_env,       // Top-Level
      *nil,           // `nil` and ()
      *tee,           // t/True
      *quote,         // quote or '
      *s_if,          // `If` symbol
      *s_lambda,      // lambda symbol
      *s_define,      // define symbol
      *s_setb;        // set! symbol

// Generate a new Cons cell from X and Y
#define cons(X, Y) omake(CONS, 2, (X), (Y))
// Get Car and Cdr of X
#define car(X) ((X)->p[0])
#define cdr(X) ((X)->p[1])

// Set Car/Cdr of X to Y
#define setcar(X, Y) (((X)->p[0]) = (Y))
#define setcdr(X, Y) (((X)->p[1]) = (Y))
// Generate INT value
#define mkint(X) omake(INT, 1, (Object*)(X))
// THIS CASTS THE VALUE OF POINTER TO OBJECT TO AN INT???
#define intval(X) ((int)((X)->p[0]))

//Make Symbol
#define mksym(X) omake(SYM, 1, (Object*)(X))
// Cast Symbol name from object pointer
#define symname(X) ((char *)((X)->p[0]))

//Generate native function 
#define mkprimop(X) omake(PRIMOP, 1, (Object*)(X))
#define primopval(X) ((primop)(X)->p[0])

// Generate Procedure
// With Args X, body Y, and Environment/Scope Z
#define mkproc(X, Y, Z) omake(PROC, 3, (X), (Y), (Z))
// get procedure args
#define procargs(X) ((X)->p[0])
// Get procedure body
#define proccode(X) ((X)->p[1])
// get procedure bindings
#define procenv(X) ((X)->p[2])
// Is symbol NIL
#define isnil(X) ((X) == nil)

// Generate a next Cons cell or linked list from
// list of Objects
Object* omake(enum ObjType type, int count, ...)
{
  // Result variable
  Object *result;
  va_list ap;
  int i;
  // varags iterations
  va_start(ap, count);
  // Alocate space for |Object_1|&Object_2|&Object_3|...|&Object_count
  result = (Object*) malloc(sizeof(Object) + (count - 1) * sizeof(Object *));
  result->type = type;
  // For item in count - 
  for (i = 0; i < count; i++)
    // This will lead to the next Object, then the Next
    result->p[i] = va_arg(ap, Object*);
  va_end(ap);
  
  return result;
}
/**Lookup symbol value from global table list
 * Returns nil if not found
*/
Object* findsym(char *name)
{
  Object *symlist;
  for (symlist = all_symbols; !isnil(symlist); symlist = cdr(symlist))
    if (!strcmp(name, symname(car(symlist))))
      return symlist;
  return nil;
}

// Add name to global symbol table
// Returns value if defined 
// Else, returns Symbol
Object* intern(char *name)
{
  Object *op = findsym(name);
  if (!isnil(op))
    return car(op);
  
  op = mksym(name);
  all_symbols = cons(op, all_symbols);
  
  return op;
}

/*** Environment ***/
// Generates list  ((Symbol Value) Environment)
#define extend(ENV, SYM, VAL) (cons(cons((SYM), (VAL)), (ENV)))
/** 
 * Recursively append symbol/value pairs to environment ENV
*/
Object *multiple_extend(Object* env, Object* syms, Object* vals)
{
  return isnil(syms) ? env : 
  multiple_extend(
      extend(env, car(syms), car(vals)), 
      cdr(syms), cdr(vals));
}
// Append (Symbol, Value) pair to top_level environment
Object *extend_top(Object *sym, Object *val)
{
  setcdr(top_env, cons(cons(sym, val), cdr(top_env)));
  return val;
}
// Not sure what this does, gets value associated with key in invironment or returns `nil`
Object* assoc(Object* key, Object* alist)
{
  if (isnil(alist))
    return nil;
  if (car(car(alist)) == key)
    return car(alist);
  return assoc(key, cdr(alist));
}

/*** Input/Output ***/
FILE* inputfile;
char *token_la;
int la_valid = 0;
#define MAXLEN 100
char buffer[MAXLEN];
int bufused;
/**
 * Append character to global buffer
*/
void add_to_buf(char ch)
{
  if (bufused < MAXLEN - 1)
    buffer[bufused++] = ch;
}
/**
 * Turn global buffer into CString, malloc's it, and returns it
*/
char* buf2str()
{
  buffer[bufused++] = '\0';
  return strdup(buffer);
}
// Assign current input file to FP
void setinput(FILE *fp) { inputfile = fp; }

void putback_token(char *token)
{
  token_la = token;
  la_valid = 1;
}

char* gettoken()
{
  int ch;

  bufused = 0;
  if (la_valid) {
    la_valid = 0;
    return token_la;
  }
  // Skip to next function
  do {
    if ((ch = getc(inputfile)) == EOF)
      exit(0);
  } while (isspace(ch));

  // Get next character 
  add_to_buf(ch);
  // If special character indicating quote or sexpr, dispatch to buf2str
  if (strchr("()\'", ch))
    return buf2str();

  for (;;) {
    if ((ch = getc(inputfile)) == EOF) exit(0);

    // If char is special character, or space, put the char back into the input stream (THIS WAS A THING)
    if (strchr("()\'", ch) || isspace(ch)) {
      ungetc(ch, inputfile);
      return buf2str();
    }
    add_to_buf(ch);
  }
}

Object *readlist();
Object *readobj()
{
  char *token;

  token = gettoken();
  if (!strcmp(token, "("))
    return readlist();
  if (!strcmp(token, "\'"))
    return cons(quote, cons(readobj(), nil));
  if (token[strspn(token, "0123456789")] == '\0')
    return mkint(atoi(token));
  return intern(token);
}

Object *readlist()
{
  char *token = gettoken();
  Object *tmp;
  if (!strcmp(token, ")"))
    return nil;
  if (!strcmp(token, "."))
  {
    tmp = readobj();
    if (strcmp(gettoken(), ")"))
      exit(1);
    return tmp;
  }
  putback_token(token);
  tmp = readobj(); /* Must force evaluation order */
  return cons(tmp, readlist());
}

void writeobj(FILE *ofp, Object *op)
{
  switch (op->type)
  {
  case INT:
    fprintf(ofp, "%d", intval(op));
    break;
  case CONS:
    fprintf(ofp, "(");
    for (;;)
    {
      writeobj(ofp, car(op));
      if (isnil(cdr(op)))
      {
        fprintf(ofp, ")");
        break;
      }
      op = cdr(op);
      if (op->type != CONS)
      {
        fprintf(ofp, " . ");
        writeobj(ofp, op);
        fprintf(ofp, ")");
        break;
      }
      fprintf(ofp, " ");
    }
    break;
  case SYM:
    if (isnil(op))
      fprintf(ofp, "()");
    else
      fprintf(ofp, "%s", symname(op));
    break;
  case PRIMOP:
    fprintf(ofp, "#<PRIMOP>");
    break;
  case PROC:
    fprintf(ofp, "#<PROC>");
    break;
  default:
    exit(1);
  }
}

/*** Evaluator (Eval/Apply) ***/
Object *evlis(Object *exps, Object *env);
Object *progn(Object *exps, Object *env);
Object *apply(Object *proc, Object *vals, Object *env);

/**
 * Evaluate expression in terms of env
*/
Object *eval(Object *exp, Object *env)
{
  Object *tmp;

  if (exp == nil)
    return nil;

  switch (exp->type)
  {
  case INT:
    return exp;
  
  case SYM:
    tmp = assoc(exp, env);
    if (tmp == nil)
      error("Unbound symbol");
    return cdr(tmp);

  case CONS:
    if (car(exp) == s_if) {
      if (eval(car(cdr(exp)), env) != nil)
        return eval(car(cdr(cdr(exp))), env);
      else
        return eval(car(cdr(cdr(cdr(exp)))), env);
    }
    if (car(exp) == s_lambda)
      return mkproc(car(cdr(exp)), cdr(cdr(exp)), env);
    if (car(exp) == quote)
      return car(cdr(exp));
    if (car(exp) == s_define)
      return (extend_top(car(cdr(exp)),
                         eval(car(cdr(cdr(exp))), env)));

    if (car(exp) == s_setb) {
      Object *pair = assoc(car(cdr(exp)), env);
      Object *newval = eval(car(cdr(cdr(exp))), env);
      setcdr(pair, newval);
      return newval;
    }
    return apply(eval(car(exp), env), evlis(cdr(exp), env), env);
  case PRIMOP:
    return exp;
  case PROC:
    return exp;
  }
  /* Not reached */
  return exp;
}

Object *evlis(Object *exps, Object *env)
{
  if (exps == nil)
    return nil;
  return cons(eval(car(exps), env),
              evlis(cdr(exps), env));
}

Object *progn(Object *exps, Object *env)
{
  if (exps == nil)
    return nil;
  for (;;)
  {
    if (cdr(exps) == nil)
      return eval(car(exps), env);
    eval(car(exps), env);
    exps = cdr(exps);
  }
}

Object *apply(Object *proc, Object *vals, Object *env)
{
  if (proc->type == PRIMOP)
    return (*primopval(proc))(vals);
  if (proc->type == PROC)
  {
    /* For dynamic scope, use env instead of procenv(proc) */
    return progn(proccode(proc),
                 multiple_extend(procenv(proc), procargs(proc), vals));
  }
  error("Bad argument to apply");
  /* Not reached */
  return nil;
}

/*** Native Functions ***/
// (+)
Object *prim_sum(Object *args)
{
  int sum;
  for (sum = 0; !isnil(args); sum += intval(car(args)), args = cdr(args))
    ;
  return mkint(sum);
}
//(-)
Object *prim_sub(Object *args) {
  int sum;
  for (sum = intval(car(args)), args = cdr(args);
       !isnil(args);
       sum -= intval(car(args)), args = cdr(args))
    ;
  return mkint(sum);
}
// (*)
Object *prim_prod(Object *args) {
  int prod;
  for (prod = 1; !isnil(args); prod *= intval(car(args)), args = cdr(args))
    ;
  return mkint(prod);
}
// (=)
Object *prim_numeq(Object *args) {
  return intval(car(args)) == intval(car(cdr(args))) ? tee : nil;
}
Object* prim_numneq(Object* args) {
  return intval(car(args)) != intval(car(cdr(args))) ? tee : nil;
}
// (>)
Object* prim_gt(Object* args){
  return intval(car(args)) > intval(car(cdr(args))) ? tee : nil;
}
// (<)
Object* prim_lt(Object* args) {
    return intval(car(args)) < intval(car(cdr(args))) ? tee : nil;
}
Object* prim_ge(Object* args){
    return intval(car(args)) >= intval(car(cdr(args))) ? tee : nil;
}
// (<=)
Object* prim_le(Object* args) {
      return intval(car(args)) <= intval(car(cdr(args))) ? tee : nil;
}
// Cons, Car, and CDR inline
Object *prim_cons(Object *args) { return cons(car(args), car(cdr(args))); }
Object *prim_car(Object *args) { return car(car(args)); }
Object *prim_cdr(Object *args) { return cdr(car(args)); }

/*** Initialization ***/
/**
 * Initialize REPL, 
 * Add values to global symbol table
 * And assign values to 
*/
void init_sl3()
{
  nil = mksym("nil");
  all_symbols = cons(nil, nil);
  top_env = cons(cons(nil, nil), nil);
  tee = intern("t");
  extend_top(tee, tee);
  quote = intern("quote");
  s_if = intern("if");
  s_lambda = intern("lambda");
  s_define = intern("define");
  s_setb = intern("set!");

  extend_top(intern("+"), mkprimop(prim_sum));
  extend_top(intern("-"), mkprimop(prim_sub));
  extend_top(intern("*"), mkprimop(prim_prod));
  extend_top(intern("="), mkprimop(prim_numeq));
  // My Extensions
  extend_top(intern(">"),  mkprimop(prim_gt));
  extend_top(intern("<"),  mkprimop(prim_lt));
  extend_top(intern(">="), mkprimop(prim_ge));
  extend_top(intern("<="), mkprimop(prim_ge));
  extend_top(intern("!="), mkprimop(prim_numneq));
  // End my extensions
  extend_top(intern("cons"), mkprimop(prim_cons));
  extend_top(intern("car"), mkprimop(prim_car));
  extend_top(intern("cdr"), mkprimop(prim_cdr));
}

/*** Main Driver ***/
int main()
{
  init_sl3();
  setinput(stdin);
  for (;;)
  {
    writeobj(stdout, eval(readobj(), top_env));
    printf("\n");
  }
  return 0;
}
