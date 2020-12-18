# Expanded Lisp

## Overview
This project is based on another I found. is a lisp implementation I discovered that was intending to act as an example of what a minimal lisp implementation should do.

I've decided to document it's behavior, and try and expand it as much as I can.

The original can be found here: www.sonoma.edu/users/l/luvisi/sl3.c  
And an explanation for it here: https://wiki.c2.com/?ImplementingLisp

## Changes
Added several native functions:
- Numeric Relational Operators`>`, `<`, `<=`, `>=`, `!=`
- Documentation for object creation
- `-` now acts as arithmetic negation if passed a single arguement