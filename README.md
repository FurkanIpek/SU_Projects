# Scheme-Interpreter
Short piece of a program for a small scaled course project

This software interprets a subset of scheme which is defined by the the following grammer:

\<s6-interpreter\> -> <expr>
      | <define>
  
<expr> -> NUMBER
      | IDENT
      | <if>
      | <let>
      | <lambda>
      | <application>

<define> -> ( define IDENT <expr> )

<if> -> ( if <expr> <expr> <expr> )

<let> -> ( let ( <var_binding_list> ) <expr> )

<letstar> -> ( let* ( <var_binding_list> ) <expr> )

<lambda> -> ( lambda ( <formal_list> ) <expr> )

<application> -> ( <operator> <operand_list> )

<operator> -> <built_in_operator>
      | <lambda>
      | IDENT
<built_in_operator> -> + | * | - | /

<operand_list> -> <expr> <operand_list>
      | empty
      
<var_binding_list> -> ( IDENT <expr> ) <var_binding_list>
      | ( IDENT <expr> )
      
<formal_list> -> IDENT <formal_list>
      | IDENT
