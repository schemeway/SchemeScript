/*
 * Copyright (c) 2004 Nu Echo Inc.
 * 
 * This is free software. For terms and warranty disclaimer, see ./COPYING
 */
package org.schemeway.plugins.schemescript.dictionary;

import java.util.*;

import org.eclipse.core.resources.IResource;

/**
 * The R5RS dictionary. Contains all symbols from the R^5RS.
 */
public class R5RSDictionary extends AbstractSymbolDictionary {
    private static Hashtable mEntries = null;
    private static R5RSDictionary mInstance = null;

    private static SymbolEntry[] mR5rsSymbols =
    {
        new SymbolEntry("define", "(define name value)", SymbolEntry.SYNTAX, SymbolEntry.HIGH),
        new SymbolEntry("begin", "(begin body ...)", SymbolEntry.SYNTAX, SymbolEntry.HIGH),
        new SymbolEntry("lambda", "(lambda formals body)", SymbolEntry.SYNTAX, SymbolEntry.HIGH),
        new SymbolEntry("if", "(if test consequent [alternative])", SymbolEntry.SYNTAX),
        new SymbolEntry("set!", "(set! var val)", SymbolEntry.SYNTAX),
        
        new SymbolEntry("boolean?", "(boolean? o)", SymbolEntry.FUNCTION),
        new SymbolEntry("not", "(not expr)", SymbolEntry.FUNCTION),
        new SymbolEntry("and", "(and expr ...)", SymbolEntry.SYNTAX),
        new SymbolEntry("or", "(or expr ...)", SymbolEntry.SYNTAX),
        new SymbolEntry("let", "(let ((var value) ...) exprs ...)", SymbolEntry.SYNTAX),
        new SymbolEntry("let*", "(let* ((var value) ...) exprs ...)", SymbolEntry.SYNTAX),
        new SymbolEntry("letrec", "(letrec ((var value) ...) exprs ...)", SymbolEntry.SYNTAX),
        new SymbolEntry("let-values", "(let-values (((var ...) value) ...) exprs ...)", SymbolEntry.SYNTAX),

        new SymbolEntry("define-syntax", "(define-syntax name clauses)", SymbolEntry.SYNTAX),
        new SymbolEntry("syntax-rules", "(syntax-rules clauses ...)", SymbolEntry.SYNTAX),

        new SymbolEntry("eq?", "(eq? o1 o2)", SymbolEntry.FUNCTION),
        new SymbolEntry("equal?", "(equal? o1 o2)", SymbolEntry.FUNCTION),
        new SymbolEntry("eqv?", "(eqv? o1 o2)", SymbolEntry.FUNCTION),
        
        new SymbolEntry("number?", "(number? obj)", SymbolEntry.FUNCTION),
        new SymbolEntry("complex?", "(complex? obj)", SymbolEntry.FUNCTION),
        new SymbolEntry("real?", "(real? o)", SymbolEntry.FUNCTION),
        new SymbolEntry("rational?", "(rational? o)", SymbolEntry.FUNCTION),
        new SymbolEntry("integer?", "(integer? o)", SymbolEntry.FUNCTION),
        new SymbolEntry("exact?", "(exact? z)", SymbolEntry.FUNCTION),
        new SymbolEntry("inexact?", "(inexact? z)", SymbolEntry.FUNCTION),
        new SymbolEntry("=", "(= z1 z2 z3 ...)", SymbolEntry.FUNCTION),
        new SymbolEntry("<", "(< z1 z2 z3 ...)", SymbolEntry.FUNCTION),
        new SymbolEntry(">", "(> z1 z2 z3 ...)", SymbolEntry.FUNCTION),
        new SymbolEntry("<=", "(<= z1 z2 z3 ...)", SymbolEntry.FUNCTION),
        new SymbolEntry(">=", "(>= z1 z2 z3 ...)", SymbolEntry.FUNCTION),
        new SymbolEntry("zero?", "(zero? z)", SymbolEntry.FUNCTION),
        new SymbolEntry("positive?", "(positive? x)", SymbolEntry.FUNCTION),
        new SymbolEntry("negative?", "(negative? x)", SymbolEntry.FUNCTION),
        new SymbolEntry("odd?", "(odd? n)", SymbolEntry.FUNCTION),
        new SymbolEntry("even?", "(even? n)", SymbolEntry.FUNCTION),
        new SymbolEntry("max", "(max x1 x2 ...)", SymbolEntry.FUNCTION),
        new SymbolEntry("min", "(min x1 x2 ...)", SymbolEntry.FUNCTION),
        new SymbolEntry("+", "(+ z1 ...)", SymbolEntry.FUNCTION),
        new SymbolEntry("*", "(* z1 ...)", SymbolEntry.FUNCTION),
        new SymbolEntry("-", "(- z1 z2 ...)", SymbolEntry.FUNCTION),
        new SymbolEntry("/", "(/ z1 z2 ...)", SymbolEntry.FUNCTION),
        new SymbolEntry("abs", "(abs x)", SymbolEntry.FUNCTION),
        new SymbolEntry("quotient", "(quotient n1 n2)", SymbolEntry.FUNCTION),
        new SymbolEntry("remainder", "(remainder n1 n2)", SymbolEntry.FUNCTION),
        new SymbolEntry("modulo", "(modulo n1 n2)", SymbolEntry.FUNCTION),
        new SymbolEntry("gcd", "(gcd n1 ...)", SymbolEntry.FUNCTION),
        new SymbolEntry("lcm", "(lcm n1 ...)", SymbolEntry.FUNCTION),
        new SymbolEntry("numerator", "(numerator q)", SymbolEntry.FUNCTION),
        new SymbolEntry("denominator", "(denominator q)", SymbolEntry.FUNCTION),
        new SymbolEntry("floor", "(floor x)", SymbolEntry.FUNCTION),
        new SymbolEntry("ceiling", "(ceiling x)", SymbolEntry.FUNCTION),
        new SymbolEntry("truncate", "(truncate x)", SymbolEntry.FUNCTION),
        new SymbolEntry("round", "(round x)", SymbolEntry.FUNCTION),
        new SymbolEntry("rationalize", "(rationalize x y)", SymbolEntry.FUNCTION),
        new SymbolEntry("exp", "(exp z)", SymbolEntry.FUNCTION),
        new SymbolEntry("log", "(log z)", SymbolEntry.FUNCTION),
        new SymbolEntry("sin", "(sin z)", SymbolEntry.FUNCTION),
        new SymbolEntry("cos", "(cos z)", SymbolEntry.FUNCTION),
        new SymbolEntry("tan", "(tan z)", SymbolEntry.FUNCTION),
        new SymbolEntry("asin", "(asin z)", SymbolEntry.FUNCTION),
        new SymbolEntry("acos", "(acos z)", SymbolEntry.FUNCTION),
        new SymbolEntry("atan", "(atan z)", SymbolEntry.FUNCTION),
        new SymbolEntry("atan", "(atan x y)", SymbolEntry.FUNCTION),
        new SymbolEntry("sqrt", "(sqrt z)", SymbolEntry.FUNCTION),
        new SymbolEntry("expt", "(expt z1 z2)", SymbolEntry.FUNCTION),
        new SymbolEntry("make-rectangular", "(make-rectangular x1 x2)", SymbolEntry.FUNCTION),
        new SymbolEntry("make-polar", "(make-polar x1 x2)", SymbolEntry.FUNCTION),
        new SymbolEntry("real-part", "(real-part z)", SymbolEntry.FUNCTION),
        new SymbolEntry("imag-part", "(imag-part z)", SymbolEntry.FUNCTION),
        new SymbolEntry("magnitude", "(magnitude z)", SymbolEntry.FUNCTION),
        new SymbolEntry("angle", "(angle z)", SymbolEntry.FUNCTION),
        new SymbolEntry("exact->inexact", "(exact->inexact z)", SymbolEntry.FUNCTION),
        new SymbolEntry("inexact->exact", "(inexact->exact z)", SymbolEntry.FUNCTION),
        new SymbolEntry("number->string", "(number->string z [radix])", SymbolEntry.FUNCTION),
        new SymbolEntry("string->number", "(string->number str [radix])", SymbolEntry.FUNCTION),

        new SymbolEntry("pair?", "(pair? o)", SymbolEntry.FUNCTION),
        new SymbolEntry("null?", "(null? o)", SymbolEntry.FUNCTION),
        new SymbolEntry("cons", "(cons o1 o2)", SymbolEntry.FUNCTION),
        new SymbolEntry("car", "(car p)", SymbolEntry.FUNCTION),
        new SymbolEntry("cdr", "(cdr p)", SymbolEntry.FUNCTION),
        new SymbolEntry("caar", "(caar p)", SymbolEntry.FUNCTION),
        new SymbolEntry("cadr", "(cadr p)", SymbolEntry.FUNCTION),
        new SymbolEntry("cdar", "(cdar p)", SymbolEntry.FUNCTION),
        new SymbolEntry("cddr", "(cddr p)", SymbolEntry.FUNCTION),
        new SymbolEntry("caaar", "(caaar p)", SymbolEntry.FUNCTION),
        new SymbolEntry("caadr", "(caadr p)", SymbolEntry.FUNCTION),
        new SymbolEntry("cadar", "(cadar p)", SymbolEntry.FUNCTION),
        new SymbolEntry("caddr", "(caddr p)", SymbolEntry.FUNCTION),
        new SymbolEntry("cdaar", "(cdaar p)", SymbolEntry.FUNCTION),
        new SymbolEntry("cdadr", "(cdadr p)", SymbolEntry.FUNCTION),
        new SymbolEntry("cddar", "(cddar p)", SymbolEntry.FUNCTION),
        new SymbolEntry("cdddr", "(cdddr p)", SymbolEntry.FUNCTION),
        new SymbolEntry("caaaar", "(caaaar p)", SymbolEntry.FUNCTION),
        new SymbolEntry("caaadr", "(caaadr p)", SymbolEntry.FUNCTION),
        new SymbolEntry("caadar", "(caadar p)", SymbolEntry.FUNCTION),
        new SymbolEntry("caaddr", "(caaddr p)", SymbolEntry.FUNCTION),
        new SymbolEntry("cadaar", "(cadaar p)", SymbolEntry.FUNCTION),
        new SymbolEntry("cadadr", "(cadadr p)", SymbolEntry.FUNCTION),
        new SymbolEntry("caddar", "(caddar p)", SymbolEntry.FUNCTION),
        new SymbolEntry("cadddr", "(cadddr p)", SymbolEntry.FUNCTION),
        new SymbolEntry("cdaaar", "(cdaaar p)", SymbolEntry.FUNCTION),
        new SymbolEntry("cdaadr", "(cdaadr p)", SymbolEntry.FUNCTION),
        new SymbolEntry("cdadar", "(cdadar p)", SymbolEntry.FUNCTION),
        new SymbolEntry("cdaddr", "(cdaddr p)", SymbolEntry.FUNCTION),
        new SymbolEntry("cddaar", "(cddaar p)", SymbolEntry.FUNCTION),
        new SymbolEntry("cddadr", "(cddadr p)", SymbolEntry.FUNCTION),
        new SymbolEntry("cdddar", "(cdddar p)", SymbolEntry.FUNCTION),
        new SymbolEntry("cddddr", "(cddddr p)", SymbolEntry.FUNCTION),
        new SymbolEntry("set-cdr!", "(set-cdr! p o)", SymbolEntry.FUNCTION),
        new SymbolEntry("list", "(list o ...)", SymbolEntry.FUNCTION),
        new SymbolEntry("list?", "(list? o)", SymbolEntry.FUNCTION),
        new SymbolEntry("length", "(length l)", SymbolEntry.FUNCTION),
        new SymbolEntry("append", "(append l ...)", SymbolEntry.FUNCTION),
        new SymbolEntry("reverse", "(reverse l)", SymbolEntry.FUNCTION),
        new SymbolEntry("list-tail", "(list-tail l k)", SymbolEntry.FUNCTION),
        new SymbolEntry("list-ref", "(list-ref l k)", SymbolEntry.FUNCTION),
        new SymbolEntry("memq", "(memq elt l)", SymbolEntry.FUNCTION),
        new SymbolEntry("memv", "(memv elt l)", SymbolEntry.FUNCTION),
        new SymbolEntry("member", "(member elt l)", SymbolEntry.FUNCTION),
        new SymbolEntry("assq", "(assq o alist)", SymbolEntry.FUNCTION),
        new SymbolEntry("assv", "(assv o alist)", SymbolEntry.FUNCTION),
        new SymbolEntry("assoc", "(assoc o alist)", SymbolEntry.FUNCTION),

        new SymbolEntry("string?", "(string? o)", SymbolEntry.FUNCTION),
        new SymbolEntry("make-string", "(make-string n [ch])", SymbolEntry.FUNCTION),
        new SymbolEntry("string", "(string ch ...)", SymbolEntry.FUNCTION),
        new SymbolEntry("string-length", "(string-length str)", SymbolEntry.FUNCTION),
        new SymbolEntry("string-ref", "(string-ref str idx)", SymbolEntry.FUNCTION),
        new SymbolEntry("string-set!", "(string-set! str idx ch)", SymbolEntry.FUNCTION),
        new SymbolEntry("string->list", "(string->list str)", SymbolEntry.FUNCTION),
        new SymbolEntry("list->string", "(list->string lst)", SymbolEntry.FUNCTION),
        new SymbolEntry("string=?", "(string=? s1 s2)", SymbolEntry.FUNCTION),
        new SymbolEntry("string<?", "(string<? s1 s2)", SymbolEntry.FUNCTION),
        new SymbolEntry("string>?", "(string>? s1 s2)", SymbolEntry.FUNCTION),
        new SymbolEntry("string<=?", "(string<=? s1 s2)", SymbolEntry.FUNCTION),
        new SymbolEntry("string>=?", "(string>=? s1 s2)", SymbolEntry.FUNCTION),
        new SymbolEntry("string-ci=?", "(string=? s1 s2)", SymbolEntry.FUNCTION),
        new SymbolEntry("string-ci<?", "(string<? s1 s2)", SymbolEntry.FUNCTION),
        new SymbolEntry("string-ci>?", "(string>? s1 s2)", SymbolEntry.FUNCTION),
        new SymbolEntry("string-ci<=?", "(string<=? s1 s2)", SymbolEntry.FUNCTION),
        new SymbolEntry("string-ci>=?", "(string>=? s1 s2)", SymbolEntry.FUNCTION),
        new SymbolEntry("string-downcase", "(string-downcase s1 s2)", SymbolEntry.FUNCTION),
        new SymbolEntry("string-upcase", "(string-upcase s1 s2)", SymbolEntry.FUNCTION),
        new SymbolEntry("substring", "(substring str start end)", SymbolEntry.FUNCTION),
        new SymbolEntry("string-append", "(string-append str ...)", SymbolEntry.FUNCTION),
        new SymbolEntry("string-copy", "(string-copy str)", SymbolEntry.FUNCTION),
        new SymbolEntry("string-fill!", "(string-fill! str ch)", SymbolEntry.FUNCTION),

        new SymbolEntry("char?", "(char? o)", SymbolEntry.FUNCTION),
        new SymbolEntry("integer->char", "(integer->char i)", SymbolEntry.FUNCTION),
        new SymbolEntry("char->integer", "(char->integer ch)", SymbolEntry.FUNCTION),
        new SymbolEntry("char=?", "(char=? ch1 ch2)", SymbolEntry.FUNCTION),
        new SymbolEntry("char<?", "(char<? ch1 ch2)", SymbolEntry.FUNCTION),
        new SymbolEntry("char>?", "(char>? ch1 ch2)", SymbolEntry.FUNCTION),
        new SymbolEntry("char<=?", "(char<=? ch1 ch2)", SymbolEntry.FUNCTION),
        new SymbolEntry("char>=?", "(char>=? ch1 ch2)", SymbolEntry.FUNCTION),
        new SymbolEntry("char-ci=?", "(char=? ch1 ch2)", SymbolEntry.FUNCTION),
        new SymbolEntry("char-ci<?", "(char<? ch1 ch2)", SymbolEntry.FUNCTION),
        new SymbolEntry("char-ci>?", "(char>? ch1 ch2)", SymbolEntry.FUNCTION),
        new SymbolEntry("char-ci<=?", "(char<=? ch1 ch2)", SymbolEntry.FUNCTION),
        new SymbolEntry("char-ci>=?", "(char>=? ch1 ch2)", SymbolEntry.FUNCTION),
        new SymbolEntry("char-upcase", "(char-upcase ch)", SymbolEntry.FUNCTION),
        new SymbolEntry("char-downcase", "(char-downcase ch)", SymbolEntry.FUNCTION),
        new SymbolEntry("char-alphabetic?", "(char-alphabetic? ch)", SymbolEntry.FUNCTION),
        new SymbolEntry("char-numeric?", "(char-numeric? ch)", SymbolEntry.FUNCTION),
        new SymbolEntry("char-whitespace?", "(char-whitespace? ch)", SymbolEntry.FUNCTION),
        new SymbolEntry("char-whitespace?", "(char-whitespace? ch)", SymbolEntry.FUNCTION),
        new SymbolEntry("char-upper-char?", "(char-upper-case? ch)", SymbolEntry.FUNCTION),
        new SymbolEntry("char-down-char?", "(char-down-case? ch)", SymbolEntry.FUNCTION),

        new SymbolEntry("symbol?", "(symbol? o)", SymbolEntry.FUNCTION),
        new SymbolEntry("symbol->string", "(symbol->string sym)", SymbolEntry.FUNCTION),
        new SymbolEntry("string->symbol", "(string->symbol str)", SymbolEntry.FUNCTION),

        new SymbolEntry("vector?", "(vector? o)", SymbolEntry.FUNCTION),
        new SymbolEntry("vector", "(vector o ...)", SymbolEntry.FUNCTION),
        new SymbolEntry("make-vector", "(make-vector n [o])", SymbolEntry.FUNCTION),
        new SymbolEntry("vector-ref", "(vector-ref v k)", SymbolEntry.FUNCTION),
        new SymbolEntry("vector-set!", "(vector-set! v k o)", SymbolEntry.FUNCTION),
        new SymbolEntry("vector-length", "(vector-length v)", SymbolEntry.FUNCTION),
        new SymbolEntry("vector-fill!", "(vector-fill! v o)", SymbolEntry.FUNCTION),
        new SymbolEntry("vector->list", "(vector->list v)", SymbolEntry.FUNCTION),
        new SymbolEntry("list->vector", "(list->vector l)", SymbolEntry.FUNCTION),

        new SymbolEntry("procedure?", "(procedure? o)", SymbolEntry.FUNCTION),
        new SymbolEntry("apply", "(apply proc o ... args)", SymbolEntry.FUNCTION),
        new SymbolEntry("call-with-current-continuation", "(call-with-current-continuation proc)", SymbolEntry.FUNCTION),
        new SymbolEntry("call/cc", "(call/cc proc)", SymbolEntry.FUNCTION),
        new SymbolEntry("map", "(map f l ...)", SymbolEntry.FUNCTION),
        new SymbolEntry("for-each", "(for-each proc l ...)", SymbolEntry.FUNCTION),
        new SymbolEntry("values", "(values o ...)", SymbolEntry.SYNTAX),
        new SymbolEntry("call-with-values", "(call-with-values producer consumer)", SymbolEntry.FUNCTION),
        new SymbolEntry("dynamic-wind", "(dynamic-wind before-thunk thunk after-thunk)", SymbolEntry.SYNTAX),
        new SymbolEntry("eval", "(eval expression env)", SymbolEntry.FUNCTION),
        new SymbolEntry("scheme-report-environment", "(scheme-report-environment version)", SymbolEntry.FUNCTION),
        new SymbolEntry("null-environment", "(null-environment version)", SymbolEntry.FUNCTION),
        new SymbolEntry("interaction-environment", "(interaction-environment version)", SymbolEntry.FUNCTION),

        new SymbolEntry("cond", "(cond (test expr ...) ...) [(else expr ...)]", SymbolEntry.SYNTAX),
        new SymbolEntry("case", "(case expr (values expr ...) ... [(else expr ...)])", SymbolEntry.SYNTAX),
        new SymbolEntry("delay", "(delay expr)", SymbolEntry.SYNTAX),
        new SymbolEntry("force", "(force expr)", SymbolEntry.FUNCTION),
        new SymbolEntry("quote", "(quote datum)", SymbolEntry.SYNTAX),
        new SymbolEntry("quasiquote", "(quasiquote datum)", SymbolEntry.SYNTAX),
        new SymbolEntry("unquote", "(unquote datum)", SymbolEntry.SYNTAX),
        new SymbolEntry("unquote-splicing", "(unquote-splicing datum)", SymbolEntry.SYNTAX),
        new SymbolEntry("load", "(load filename)", SymbolEntry.FUNCTION),
        new SymbolEntry("open-input-file", "(open-input-file filename)", SymbolEntry.FUNCTION),
        new SymbolEntry("open-output-file", "(open-output-file filename)", SymbolEntry.FUNCTION),
        new SymbolEntry("close-input-port", "(close-input-port iport)", SymbolEntry.FUNCTION),
        new SymbolEntry("close-output-port", "(close-output-port oport)", SymbolEntry.FUNCTION),
        new SymbolEntry("display", "(display obj [port])", SymbolEntry.FUNCTION),
        new SymbolEntry("write", "(write obj [port])", SymbolEntry.FUNCTION),
        new SymbolEntry("write-char", "(write-char ch [port])", SymbolEntry.FUNCTION),
        new SymbolEntry("newline", "(newline [port])", SymbolEntry.FUNCTION),
        new SymbolEntry("read", "(read [port])", SymbolEntry.FUNCTION),
        new SymbolEntry("read-char", "(read-char [port])", SymbolEntry.FUNCTION),
        new SymbolEntry("peek-char", "(peek-char [port])", SymbolEntry.FUNCTION),
        new SymbolEntry("char-ready?", "(char-ready? [port])", SymbolEntry.FUNCTION),
        new SymbolEntry("eof-object?", "(eof-object? obj)", SymbolEntry.FUNCTION),
        new SymbolEntry("with-input-from-file", "(with-input-from-file filename thunk)", SymbolEntry.FUNCTION),
        new SymbolEntry("with-output-to-file", "(with-output-to-file filename thunk)", SymbolEntry.FUNCTION),
        new SymbolEntry("call-with-input-file", "(call-with-input-file filename procedure)", SymbolEntry.FUNCTION),
        new SymbolEntry("call-with-output-file", "(call-with-output-file filename procedure)", SymbolEntry.FUNCTION),
        new SymbolEntry("call-with-input-string", "(call-with-input-string string procedure)", SymbolEntry.FUNCTION),
        new SymbolEntry("call-with-output-string", "(call-with-output-string procedure)", SymbolEntry.FUNCTION),
        new SymbolEntry("current-input-port", "(current-input-port)", SymbolEntry.FUNCTION),
        new SymbolEntry("current-output-port", "(current-output-port)", SymbolEntry.FUNCTION),
        new SymbolEntry("current-error-port", "(current-error-port)", SymbolEntry.FUNCTION)

    };

    private R5RSDictionary() {
        super();
    }

    public static final R5RSDictionary getInstance() {
        if (mInstance == null) {
            mInstance = new R5RSDictionary();
        }
        return mInstance;
    }

    protected void findSymbols(String name, List entries) {
        SymbolEntry entry = (SymbolEntry) mEntries.get(name);
        if (entry != null)
            entries.add(entry);
    }

    protected void completeSymbols(String prefix, List entries) {
        Enumeration enumeration = mEntries.keys();
        while (enumeration.hasMoreElements()) {
            String symbolName = (String) enumeration.nextElement();
            if (symbolName.startsWith(prefix)) {
                entries.add((SymbolEntry) mEntries.get(symbolName));
            }
        }
    }

    static {
        mEntries = new Hashtable();
        for (int index = 0; index < mR5rsSymbols.length; index++) {
            SymbolEntry entry = mR5rsSymbols[index];
            mEntries.put(entry.getName(), entry);
        }
    }

    public List findSymbolForResource(IResource resource)
    {
        return new LinkedList();
    }
}