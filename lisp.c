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
enum ObjType {
  INT,
  SYM,
  CONS,
  PROC,
  PRIMOP
};
/**
 * Type Object
 * This is a tagged union with multiple members, represented by a flexible struct member -- each object-type holds between 1-3 elements
 * Special elements like `t` and `nil` are represented by global symbols to allow for pointer comparison
*/
// Forward declaration
struct sObject;

/**
 * PrimOp
 * Function pointer to a native function 
*/
typedef struct sObject *(*PrimOp)(struct sObject*);

/**
 * Value
 * Represents any of the data types that an Object has a payload 
*/
typedef union {
  int number;
  char* string;
  struct sObject* object;
  PrimOp primitive;
  // TODO rest
} Value;


typedef struct sObject
{
  enum ObjType type;
  Value data[]; // Currently same trcik
} Object;


/*** Global Constants***/
unsigned int global_counter = 0;
Object *all_symbols,  // Global symbol table
      *top_env,       // Top-Level
      *nil,           // `nil` and ()
      *tee,           // t/True
      *quote,         // quote or '
      *s_if,          // `If` symbol
      *s_lambda,      // lambda symbol
      *s_define,      // define symbol
      *s_setb;        // set! symbol
/*** MACROS ***/
//Wrapper for debugging allocations
#ifdef DEBUG_SHOW_ALLOC
#define omake(...) ( \
      printf("\033[0;32mAllocating object #%03d in %s[%d]\033[0m\n", global_counter++,__func__, __LINE__), \
      omake_function(__VA_ARGS__)\
)
#endif

// Generate a new Cons cell from X and Y
#define cons(X, Y) omake(CONS, 2, (X), (Y))
// Get Car and Cdr of X
#define car(X) ((X)->data[0].object)
#define cdr(X) ((X)->data[1].object)

// Set Car/Cdr of X to Y
#define setcar(X, Y) (((X)->data[0].object) = (Y))
#define setcdr(X, Y) (((X)->data[1].object) = (Y))
// Generate INT value
#define mkint(X) omake(INT, 1, (X))
// THIS CASTS THE VALUE OF POINTER TO OBJECT TO AN INT???
#define intval(X) ((X)->data[0].number)

//Make Symbol
#define mksym(X) omake(SYM, 1, (X))
// Cast Symbol name from object pointer
#define symname(X) (((X)->data[0].string))

//Generate native function 
#define mkprimop(X) omake(PRIMOP, 1, (X))
#define primopval(X) ((X)->data[0].primitive)

// Generate Procedure
// With Args X, body Y, and Environment/Scope Z
#define mkproc(X, Y, Z) omake(PROC, 3, (X), (Y), (Z))
// get procedure args
#define procargs(X) ((X)->data[0].object)
// Get procedure body
#define proccode(X) ((X)->data[1].object)
// get procedure bindings
#define procenv(X) ((X)->data[2].object)
// Is symbol NIL
#define isnil(X) ((X) == nil)

// Generate a next Cons cell or linked list from
// list of Objects
#ifdef DEBUG_SHOW_ALLOC
Object* omake_function(enum ObjType type, int count, ...)
#else 
Object* omake_function(enum ObjType type, int count, ...)
#endif
{
  // Result variable
  Object *result;
  va_list ap;

  // varags iterations
  va_start(ap, count);
  // Alocate space for |Object_1|&Object_2|&Object_3|...|&Object_count
  result = (Object*) malloc(sizeof(Object) + count * sizeof(Value));
  result->type = type;
  switch (type) {
    case INT:
      result->data[0].number = va_arg(ap, int);
      break;
    
    case SYM:
      result->data[0].string = va_arg(ap, char*);
      break;
    
    case CONS:
      result->data[0].object = va_arg(ap, Object*);
      result->data[1].object = va_arg(ap, Object*);
      break;
    
    case PROC:
      result->data[0].object = va_arg(ap, Object*);
      result->data[1].object = va_arg(ap, Object*);
      result->data[2].object = va_arg(ap, Object*);
      break;

    case PRIMOP:
      result->data[0].primitive = va_arg(ap, PrimOp);
      break;

    default:
      error("Not Currently Suported!");
  }
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
Object *multiple_extend(Object* environment, Object* symbols, Object* values)
{
  return isnil(symbols) ? environment : 
  multiple_extend(
      extend(environment, car(symbols), car(values)), 
      cdr(symbols), cdr(values));
}

// Append (Symbol, Value) pair to top_level environment
Object *extend_top(Object* symbol, Object* values)
{
  setcdr(top_env, cons(cons(symbol, values), cdr(top_env)));
  return values;
}

// Recursively looks up value of key in alist which is used as a dict -- if the list if nil it returns nil 
Object* lookup(Object* key, Object* alist)
{
  if (isnil(alist))
    return nil;
  if (car(car(alist)) == key)
    return car(alist);
  return lookup(key, cdr(alist));
}

/*** Input/Output ***/
//Current inputfile, normally stdin
FILE* inputfile;
// token_last is the last token, last_valid is set if a token has been pushed into token_last
// Used for backtracking when checking for end of list
char *token_last;
int last_valid = 0;
// Buffer here is used for storing strings beforing their heap allocated as tokens (for parsing) or symbols
#define MAXLEN 100
char buffer[MAXLEN];
// Number of characters in buffer, used for allocation
int buffer_used;
/**
 * Append character to global buffer
*/
void add_to_buf(char ch) {
  if (buffer_used < MAXLEN - 1)
    buffer[buffer_used++] = ch;
}

/**
 * Turn global buffer into CString, malloc's it, and returns it
*/
char* buf2str() {
  buffer[buffer_used++] = '\0';
  return strdup(buffer);
}
// Assign current input file to FP
void setinput(FILE *fp) { inputfile = fp; }

// Backtracking
// By pushing back one token
void putback_token(char *token) {
  token_last = token;
  last_valid = 1;
}

/**
 * Retrieve next caracter, ignoring comments
*/
int nextchar() {
  int ch;
  ch = getc(inputfile);
  if (ch == ';') {
    do {
      ch = getc(inputfile);
      // if either \n or EOF is found, then break;
    } while (ch != '\n' && ch != EOF);
  }

  return ch;
}
// This returns a String as a char* representing the next token in the input scheme
char* gettoken() {
  int ch;

  // Initiates buffer size
  buffer_used = 0;
  // TODO I have no clue what this means
  if (last_valid) {
    last_valid = 0;
    return token_last;
  }
  // Skip to next token
  do {
    if ((ch = nextchar()) == EOF)
      exit(0);
  } while (isspace(ch));

  // Store next character in buffer
  add_to_buf(ch);
  // if ch is equal to special characters () or ', return them (I suppose since their one letter tokens)
  if (strchr("()\'", ch)) // if ch in strchr
    return buf2str();


  // Next, we are going to iteratively read in the characters of the next token
  for (;;) {
    // Read next character into ch
    // Exit if you reach end of Input file
    if ((ch = nextchar()) == EOF) exit(0);

    // Is the next character in ch is a space or one of the special terminating characters like ' or ()
    // Push the ch back into the input stream (so that it can be read on next pass?)
    // And return token
    if (strchr("()\'", ch) || isspace(ch)) {
      ungetc((int) ch, inputfile);
      return buf2str();
    }
    // Add that element to buffer
    add_to_buf(ch);
  }
}

Object* readlist();

// Reads next object (including Sexprs) from input and returns in
// TODO gc should handle the elements gotten here, either in the return or not
Object *readobj() {
  char* token;

  token = gettoken();
  // If '(', start reading in Sexprs
  if (!strcmp(token, "("))
    return readlist();
  // Elif "'", create a quote value with next read object
  if (!strcmp(token, "\'"))
    return cons(quote, cons(readobj(), nil)); // GC will handle this case
  // This reads in token int
  // TODO what is this??
  if (token[strspn(token, "0123456789")] == '\0')
    return mkint(atoi(token));
  return intern(token);
}

Object* readlist()
{
  char *token = gettoken();
  Object *tmp;
  // Handle '()' nil value
  if (!strcmp(token, ")")) return nil;

  /**
   * This is commentd out because I found that it simply doesn't work very well with the interpreter otherwise
   * And I think it was originally meant to allow inline (1 . 2) cells, but I'm removing it because I don't it make sense otherwise
  */
  // if ((!strcmp(token, ".")) && count == 1){ // if token = "."
  //   tmp = readobj();        // Read the next object
  //   if (strcmp(gettoken(), ")")) error("cons cell dot format only allows 2 elements!"); // If the token after it is not ')' exit??
  //   return tmp;
  // }

  // Backtrace
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
Object *eval_list(Object *exps, Object *env);
Object *eval_many(Object *exps, Object *env);
Object *apply(Object *proc, Object *vals, Object *env);

/**
 * Evaluate `expression` in terms of `environment`
 * note `environment` is almost always top_level
*/
Object* eval(Object *expression, Object *environment) {
    Object* tmp;

    if (expression == nil)
      return nil;

    switch (expression->type){
    case INT:
      return expression;
    
    case SYM:
      tmp = lookup(expression, environment);
      if (tmp == nil)
        error("Unbound symbol");
      return cdr(tmp);

    case CONS:
      if (car(expression) == s_if) {
        if (eval(car(cdr(expression)), environment) != nil)
          return eval(car(cdr(cdr(expression))), environment);
        else
          return eval(car(cdr(cdr(cdr(expression)))), environment);
      }
    // Generate a lambda value
      if (car(expression) == s_lambda)
        return mkproc(
            car(cdr(expression)), // Parameters
            cdr(cdr(expression)), // 
            environment);

    // Return value of quote
      if (car(expression) == quote)
        return car(cdr(expression));
    // Set global value of symbol
      if (car(expression) == s_define)
        return (extend_top(car(cdr(expression)),
                          eval(car(cdr(cdr(expression))), environment)));

      if (car(expression) == s_setb) {
        Object* name = lookup(car(cdr(expression)), environment);
        Object* newval = eval(car(cdr(cdr(expression))), environment);
        setcdr(name, newval);
        return newval;
      }
    // If no special forms detected
    // Evaluate it as a procedure call
      return apply(
          eval(car(expression), environment), 
          eval_list(cdr(expression), environment), 
          environment);
    case PRIMOP:
      return expression;
    case PROC:
      return expression;
  }
  /* Not reached */
  return expression;
}
/**
 * Evaluates all of the objects in a list and returns a list of their results
 * e.g. (s1 s2 s3) => (1 2 3) if s1=1, s2=2 and s3=3
*/
Object* eval_list(Object *expressions, Object *environment) {
  if (expressions == nil) return nil;

  return cons(eval(car(expressions), environment),
              eval_list(cdr(expressions), environment));
}
/**
 * Eval multiple expressions, and returns the last one, used for evaluaty lambda body expressions
 * Equivalent to `progn` special form
 * See https://www.gnu.org/software/emacs/manual/html_node/eintr/progn.html
*/
Object* eval_many(Object *exps, Object *env) {
  if (exps == nil)
    return nil;
  for (;;){
    if (cdr(exps) == nil)
      return eval(car(exps), env);
    eval(car(exps), env);
    exps = cdr(exps);
  }
}
/**
 * Apply procedure to arguements in terms of environment 
*/
Object* apply(Object* procedure, Object* arguements, Object* environment) {
  // Curious, if the function is a builtin, env is not passed to it -- I wonder why?
  if (procedure->type == PRIMOP)
    return (*primopval(procedure))(arguements);
  // 
  if (procedure->type == PROC) {
    /* For dynamic scope, use env instead of procenv(proc) */
    return eval_many(proccode(procedure),
                 multiple_extend(procenv(procedure), 
                                  procargs(procedure), 
                                  arguements)
                  );
  }
  error("Bad argument to apply");
  /* Not reached */
  return nil;
}

/*** Native Functions ***/
// (+)
Object* prim_sum(Object *args) {
  int sum;
  for (sum = 0; !isnil(args); sum += intval(car(args)), args = cdr(args))
    ;
  return mkint(sum);
}
//(-)
Object *prim_sub(Object *args) {
  int sum;

  if (!isnil(car(args))  &&  isnil(cdr(args))){
    return mkint(0 - intval(car(args)));
  }

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
void init_sl3() {
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
int main() {
  init_sl3();
  setinput(stdin);
  for (;;)
  {
    writeobj(stdout, eval(readobj(), top_env));
    printf("\n");
  }
  return 0;
}
